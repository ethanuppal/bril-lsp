// Copyright (C) 2024 Ethan Uppal.
//
// This program is free software: you can redistribute it and/or modify it under
// the terms of the GNU General Public License as published by the Free Software
// Foundation, version 3 of the License only.
//
// This program is distributed in the hope that it will be useful, but WITHOUT
// ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
// FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
// details.
//
// You should have received a copy of the GNU General Public License along with
// this program.  If not, see <https://www.gnu.org/licenses/>.

use std::{
    fmt,
    ops::{Deref, DerefMut},
};

pub use logos::Span;

#[derive(Debug, Clone)]
pub struct Loc<T> {
    pub inner: T,
    pub span: Span,
}

impl<T> Loc<T> {
    pub fn new(inner: T, span: Span) -> Self {
        Self { inner, span }
    }

    pub fn without_inner(&self) -> Loc<()> {
        Loc::new((), self.span.clone())
    }

    pub fn map<U, F: FnOnce(T) -> U>(self, f: F) -> Loc<U> {
        Loc::new(f(self.inner), self.span)
    }
}

impl<T> Deref for Loc<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

impl<T> DerefMut for Loc<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.inner
    }
}

impl<T: fmt::Display> fmt::Display for Loc<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.inner.fmt(f)
    }
}

pub trait Spanned {
    fn span(&self) -> Span;
}

impl Spanned for Span {
    fn span(&self) -> Span {
        self.clone()
    }
}

impl Spanned for &Span {
    fn span(&self) -> Span {
        (*self).clone()
    }
}

impl<T> Spanned for Loc<T> {
    fn span(&self) -> Span {
        self.span.clone()
    }
}

impl<T> Spanned for &Loc<T> {
    fn span(&self) -> Span {
        self.span.clone()
    }
}

pub trait WithLocation {
    fn at(self, spanned: impl Spanned) -> Loc<Self>
    where
        Self: Sized,
    {
        Loc::new(self, spanned.span())
    }

    fn between(self, start: impl Spanned, end: impl Spanned) -> Loc<Self>
    where
        Self: Sized,
    {
        Loc::new(self, start.span().start..end.span().end)
    }
}

impl<T> WithLocation for T {}

pub trait MaybeSpanned {
    fn try_span(&self) -> Option<Span>;
}

impl<T: Spanned> MaybeSpanned for T {
    fn try_span(&self) -> Option<Span> {
        Some(self.span())
    }
}

impl<T: Spanned> MaybeSpanned for Vec<T> {
    fn try_span(&self) -> Option<Span> {
        if self.is_empty() {
            None
        } else {
            Some(
                self.first().unwrap().span().start
                    ..self.last().unwrap().span().end,
            )
        }
    }
}

impl<T: Spanned> MaybeSpanned for Option<T> {
    fn try_span(&self) -> Option<Span> {
        self.as_ref().map(|inner| inner.span())
    }
}
