import sys, subprocess
import multiprocessing


def init_worker(shared_failure_event):
    global failure_event
    failure_event = shared_failure_event


def check_file(file):
    print(f"\x1b[33m{file} START\x1b[m")

    result1 = subprocess.run(
        f"cargo run --quiet --bin brilfmt -- {file}",
        shell=True,
        capture_output=True,
        text=True,
    )
    if result1.returncode != 0:
        print(f"\x1b[31m{file} ERROR\x1b[m")
        print(result1.stderr)
        failure_event.set()
        return

    result2 = subprocess.run(
        f"cargo run --quiet --bin brilfmt -- {file}",
        shell=True,
        capture_output=True,
        text=True,
    )
    if result2.returncode != 0:
        print(f"\x1b[31m{file} ERROR\x1b[m")
        print(result2.stderr)
        failure_event.set()
        return

    if result1.stdout == result2.stdout:
        print(f"\x1b[32m{file} OK\x1b[m")
    else:
        print(f"\x1b[31m{file} NOT IDEMPOTENT\x1b[m")
        failure_event.set()


if __name__ == "__main__":
    if len(sys.argv) < 2:
        print("usage: python3 check.py <files>...")
        sys.exit(1)

    files = sys.argv[1:]

    with multiprocessing.Manager() as manager:
        failure_event = manager.Event()

        with multiprocessing.Pool(
            multiprocessing.cpu_count(),
            initializer=init_worker,
            initargs=(failure_event,),
        ) as pool:
            pool.imap_unordered(check_file, files)
            pool.close()
            pool.join()
            if failure_event.is_set():
                sys.exit(1)
