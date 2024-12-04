use crate::util::{Grid, Point2};
use ratatui::{
    buffer::Buffer,
    layout::Rect,
    style::{Color, Modifier, Style},
    text::{Line, Span, Text},
    widgets::{Paragraph, Widget},
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
    fn render(self, area: Rect, buf: &mut Buffer)
    where
        Self: Sized,
    {
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
        Paragraph::new(text).scroll((100, 0)).render(area, buf);
    }
}
