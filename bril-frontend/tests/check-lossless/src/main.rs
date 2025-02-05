use std::{
    backtrace::Backtrace, ffi::OsString, fs, io::Write, process::{Command, Stdio}, sync::mpsc, thread::available_parallelism
};

use argh::FromArgs;
use camino::Utf8PathBuf;
use owo_colors::OwoColorize;
use snafu::{whatever, OptionExt, ResultExt, Snafu};
use threadpool::ThreadPool;

#[derive(Debug, Snafu)]
#[snafu(whatever)]
#[snafu(display("{message}"))]
pub struct Whatever {
    #[snafu(source(from(Box<dyn std::error::Error + Send + Sync>, Some)))]
    #[snafu(provide(false))]
    source: Option<Box<dyn std::error::Error + Send + Sync>>,
    message: String,
    backtrace: Backtrace,
}

/// checks lossless-ness of bril-frontend
#[derive(FromArgs)]
struct Opts {
    /// sh executable
    #[argh(option, default = "\"sh\".into()")]
    sh: OsString,

    /// cargo executable
    #[argh(option, default = "\"cargo\".into()")]
    cargo: OsString,

    /// bril2json executable
    #[argh(option, default = "\"bril2json\".into()")]
    bril2json: OsString,

    /// paths to textual bril files
    #[argh(positional)]
    paths: Vec<Utf8PathBuf>,

    /// ignore paths with these suffixes
    #[argh(option)]
    exclude: Vec<String>,
}

#[snafu::report]
fn main() -> Result<(), Whatever> {
    let opts: Opts = argh::from_env();

    let worker_count = available_parallelism().map(|value| value.get()).unwrap_or(4);
    let pool = ThreadPool::new(worker_count);

    let (tx, rx) = mpsc::channel();
    let paths = opts
            .paths
            .into_iter()
            .filter(|path| !opts.exclude.iter().any(|exclude| path.ends_with(exclude))).collect::<Vec<_>>();
    for path in paths.clone() {
        let sh = opts.sh.clone();
        let cargo = opts.cargo.to_string_lossy().to_string();
        let bril2json = opts.bril2json.to_string_lossy().to_string();
        let tx = tx.clone();
        let path_clone = path.clone();
        pool.execute(move || {
            let try_block = move || -> Result<Utf8PathBuf, Whatever> {
                let contents = fs::read_to_string(&path)
                    .whatever_context(format!("Failed to read {}", path))?;
                let mut bril2json_child = Command::new(&bril2json)
                    .stdin(Stdio::piped())
                    .stdout(Stdio::piped())
                    .spawn()
                    .whatever_context(format!(
                        "Failed to spawn bril2json: invoked `{} < {}`",
                        bril2json,
                        path
                    ))?;
                bril2json_child.stdin.as_mut().whatever_context("Couldn't access the standard input of the bril2json child process")?.write_all(contents.as_bytes()).whatever_context("Failed to write to standard input")?;
                let bril2json_output = bril2json_child
                    .wait_with_output()
                    .whatever_context("Failed to finish executing bril2json")?;

                let bril2json_stdout = 
                        String::from_utf8(bril2json_output.stdout).unwrap_or_default();
                if !bril2json_output.status.success() {
                    whatever!(
                        "Invocation of `bril2json <{path}` failed with nonzero exit code: {}\n\n--- STDOUT ---\n{}\n\n--- STDERR ---\n{}",
                        bril2json_output.status,
                        bril2json_stdout,
                        String::from_utf8(bril2json_output.stderr).unwrap_or_default()
                    );
                }

                let bril2json_bril: serde_json::Value = serde_json::from_str(&bril2json_stdout).whatever_context("Failed to parse bril2json output as valid JSON")?;

                // too lazy to be granular here
                let our_output = Command::new(sh).args(["-c", &format!("{cargo} run --quiet --example print -- {path} | {bril2json}")]).output().whatever_context(format!("Failed to execute the lossless printer on {}", path))?;
                
                let our_stdout = 
                        String::from_utf8(our_output.stdout).unwrap_or_default();
                if !our_output.status.success() {
                    whatever!(
                        "Invocation of `cargo run --quiet --example print -- {path} | {bril2json}` failed with nonzero exit code: {}\n\n--- STDOUT ---\n{}\n\n--- STDERR ---\n{}",
                        our_output.status,
                        our_stdout,
                        String::from_utf8(our_output.stderr).unwrap_or_default()
                    );
                }

                let our_bril: serde_json::Value = serde_json::from_str(&bril2json_stdout).whatever_context("Failed to parse our output as valid JSON")?;

                if our_bril != bril2json_bril {
                    whatever!("Mismatch:
--EXPECTED--
{bril2json_bril}

--RECEIVED--
{our_bril}");
                }


                Ok(path)
            };
            tx.send(try_block().map_err(|error| (path_clone, error))).expect("failed to send over channel");
        });
    }

    let mut has_error = false;
    let mut number_received = 0;
    while number_received < paths.len() {
        match rx.recv().expect("failed to receive from channel") {
    Ok(path) => {
                println!("{}", format!("{path} OK").bold().bright_green());
            },
    Err((path, error)) => {
                println!("{}", format!("{path} ERROR").bold().bright_red());
                eprintln!("{path} ERROR:\n{error}");
                has_error = true;
            },
};
        number_received += 1;
    }

    if has_error {
        whatever!("Exiting due to error(s)");
    }

    Ok(())
}
