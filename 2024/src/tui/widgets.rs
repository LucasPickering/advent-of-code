use crate::util::{Direction, Grid, Point2};
use ratatui::{
    buffer::Buffer,
    layout::Rect,
    style::{Color, Modifier, Style},
    text::{Line, Span, Text},
    widgets::{Paragraph, StatefulWidget, Widget},
};
use std::fmt::Display;

const STYLE_RED: Style = Style::new()
    .fg(Color::Red)
    .add_modifier(Modifier::BOLD)
    .add_modifier(Modifier::UNDERLINED);
const STYLE_GREEN: Style = Style::new()
    .fg(Color::Green)
    .add_modifier(Modifier::BOLD)
    .add_modifier(Modifier::UNDERLINED);

/// Render a grid with some cells selected
pub struct GridWidget<'a, T> {
    pub grid: &'a Grid<T>,
    pub selected_red: &'a [Point2<usize>],
    pub selected_green: &'a [Point2<usize>],
}

impl<T: Display> Widget for GridWidget<'_, T> {
    fn render(self, area: Rect, buf: &mut Buffer) {
        let lines = self.grid.rows().map(|row| {
            row.map(|(point, value)| {
                if self.selected_red.contains(&point) {
                    Span::styled(value.to_string(), STYLE_RED)
                } else if self.selected_green.contains(&point) {
                    Span::styled(value.to_string(), STYLE_GREEN)
                } else {
                    value.to_string().into()
                }
            })
            .collect::<Line>()
        });
        let text = Text::from_iter(lines);
        Paragraph::new(text).scroll((0, 0)).render(area, buf);
    }
}

pub struct PanWidget;

impl StatefulWidget for PanWidget {
    type State = PanState;

    fn render(self, area: Rect, buf: &mut Buffer, state: &mut Self::State) {
        // Copy just the visible stuff to our buffer
        // This is inefficient for very large children because it renders even
        // stuff out of frame, but it's a simple and generic solution
        for x in 0..area.width {
            for y in 0..area.height {
                buf[(x, y)] = state.buffer[(state.x + x, state.y + y)].clone();
            }
        }
    }
}

#[derive(Clone, Debug, Default)]
pub struct PanState {
    x: u16,
    y: u16,
    buffer: Buffer,
}

impl PanState {
    pub fn draw(&mut self, widget: impl Widget) {
        let max_area = Rect {
            x: 0,
            y: 0,
            width: 1000,
            height: 1000,
        };
        self.buffer = Buffer::empty(max_area);
        widget.render(max_area, &mut self.buffer);
    }

    pub fn pan(&mut self, direction: Direction, num: u16) {
        match direction {
            Direction::Up => self.y = self.y.saturating_sub(num),
            Direction::Right => self.x = self.x.saturating_add(num),
            Direction::Down => self.y = self.y.saturating_add(num),
            Direction::Left => self.x = self.x.saturating_sub(num),
        }
    }
}
