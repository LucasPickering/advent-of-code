mod widgets;

pub use widgets::GridWidget;

use crate::{
    tui::widgets::{PanState, PanWidget},
    util::Direction,
};
use crossterm::event::{Event, KeyCode, KeyEvent, KeyEventKind, KeyModifiers};
use ratatui::{
    prelude::{CrosstermBackend, Terminal},
    widgets::Widget,
};
use std::io::Stdout;

type Term = Terminal<CrosstermBackend<Stdout>>;

pub struct Tui {
    terminal: Option<Term>,
    level: Level,
    to_skip: usize,
    pan_state: PanState,
}

impl Tui {
    pub fn new(level: Level) -> Self {
        let terminal = if level > Level::None {
            Some(ratatui::init())
        } else {
            None
        };
        Self {
            terminal,
            level,
            pan_state: PanState::default(),
            to_skip: 0,
        }
    }

    pub fn show(&mut self, level: Level, widget: impl Widget) {
        let terminal = match self.terminal.as_mut() {
            Some(terminal) if self.level >= level => terminal,
            // TUI is disabled
            _ => return,
        };
        if self.to_skip > 0 {
            self.to_skip -= 1;
            return;
        }

        self.pan_state.draw(widget);

        loop {
            terminal
                .draw(|frame| {
                    frame.render_stateful_widget(
                        PanWidget,
                        frame.area(),
                        &mut self.pan_state,
                    )
                })
                .expect("Error drawing TUI");
            if let Event::Key(KeyEvent {
                kind: KeyEventKind::Press,
                code,
                modifiers,
                ..
            }) = crossterm::event::read().unwrap()
            {
                match code {
                    // Zoom to the end
                    KeyCode::Char('q') | KeyCode::Esc => {
                        self.quit();
                        return;
                    }
                    KeyCode::Char('c')
                        if modifiers == KeyModifiers::CONTROL =>
                    {
                        self.quit();
                        return;
                    }
                    KeyCode::Up => self.pan_state.pan(Direction::Up, 1),
                    KeyCode::Down => self.pan_state.pan(Direction::Down, 1),
                    KeyCode::Left => self.pan_state.pan(Direction::Left, 1),
                    KeyCode::Right => self.pan_state.pan(Direction::Right, 1),
                    KeyCode::PageUp => self.pan_state.pan(Direction::Up, 20),
                    KeyCode::PageDown => {
                        self.pan_state.pan(Direction::Down, 20)
                    }
                    // Carry on with our work
                    KeyCode::Char(' ') => return,
                    _ => {}
                }
            }
        }
    }

    fn quit(&mut self) {
        self.level = Level::None;
        ratatui::restore();
    }
}

impl Drop for Tui {
    fn drop(&mut self) {
        if self.terminal.is_some() {
            ratatui::restore();
        }
    }
}

/// Verbosity level for TUI interactions
#[derive(
    Copy,
    Clone,
    Debug,
    derive_more::Display,
    PartialEq,
    PartialOrd,
    Eq,
    Ord,
    clap::ValueEnum,
)]
pub enum Level {
    #[display("none")]
    None,
    #[display("info")]
    Info,
    #[display("debug")]
    Debug,
    #[display("trace")]
    Trace,
}
