use std::ops::Range;

use ariadne::{Color, Label, Report, ReportKind, Source};

/// A byte range in the original Rune source. Ariadne converts this to line and
/// column coordinates when rendering a diagnostic.
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct Span(pub Range<usize>);

impl Span {
    pub fn new(start: usize, end: usize) -> Self {
        Self(start..end.max(start))
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Phase {
    Lexing,
    Parsing,
    SemanticAnalysis,
    CodeGeneration,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Diagnostic {
    pub phase: Phase,
    pub message: String,
    pub span: Span,
}

impl Diagnostic {
    pub fn new(phase: Phase, message: impl Into<String>, span: Span) -> Self {
        Self {
            phase,
            message: message.into(),
            span,
        }
    }

    pub fn render(&self, filename: &str, source: &str) -> String {
        let start = self.span.0.start.min(source.len());
        let end = self.span.0.end.min(source.len()).max(start);
        let phase = match self.phase {
            Phase::Lexing => "lexing",
            Phase::Parsing => "parsing",
            Phase::SemanticAnalysis => "semantic analysis",
            Phase::CodeGeneration => "code generation",
        };
        let report = Report::build(ReportKind::Error, (filename, start..end))
            .with_message(format!("{} error: {}", phase, self.message))
            .with_label(
                Label::new((filename, start..end))
                    .with_message(self.message.clone())
                    .with_color(Color::Red),
            )
            .finish();
        let mut output = Vec::new();
        if report
            .write((filename, Source::from(source)), &mut output)
            .is_ok()
        {
            String::from_utf8_lossy(&output).into_owned()
        } else {
            format!("{} error: {}", phase, self.message)
        }
    }
}

impl std::fmt::Display for Diagnostic {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{} error: {}",
            match self.phase {
                Phase::Lexing => "lexing",
                Phase::Parsing => "parsing",
                Phase::SemanticAnalysis => "semantic analysis",
                Phase::CodeGeneration => "code generation",
            },
            self.message
        )
    }
}

impl std::error::Error for Diagnostic {}
