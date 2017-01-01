extern crate piston_window;

use piston_window::*;

trait Distanceable {
    fn simple_distance_from(&self, other: &Self) -> f64;
    fn distance_from(&self, other: &Self) -> f64;
}

#[derive(Clone, Copy, Debug)]
struct Point {
    x: f64,
    y: f64,
}

#[derive(Clone, Copy, Debug)]
struct Geo {
    w: f64,
    h: f64,
}

impl std::fmt::Display for Point {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "({}, {})", self.x, self.y)
    }
}

impl std::fmt::Display for Geo {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "[{} x {}]", self.w, self.h)
    }
}

impl Distanceable for Point {
    fn simple_distance_from(&self, other: &Self) -> f64 {
        (self.x - other.x).abs() + (self.y - other.y).abs()
    }

    fn distance_from(&self, other: &Self) -> f64 {
        unimplemented!();
    }
}

struct ViewportManager {
    vp_pos: Point,  // world coords
    vp_geo: Geo,  // world coords
}

enum ConwayCell {
    Alive,
    NotAlive,
}

struct World {
    arr: Vec<ConwayCell>,
}

enum ManagedInputEvent {
    Click { x: f64, y: f64 },
    Drag { dx: f64, dy: f64 },
    Scroll { dx: f64, dy: f64 },
    Zoom { df: f64 },
}

struct InputManager {
    mouse_pos: Point,
    mouse_depressed: bool,
    mouse_dragged_tot: f64,
    ctrl_depressed: bool,
}

impl InputManager {
    fn new() -> Self {
        InputManager {
            mouse_pos: Point { x: 0.0, y: 0.0 },
            mouse_depressed: false,
            mouse_dragged_tot: 0.0,
            ctrl_depressed: false,
        }
    }

    fn handle_input_event(&mut self, input: &Input) {
        match *input {
            Input::Move(Motion::MouseCursor(x, y)) => {
                self.mouse_pos.x = x;
                self.mouse_pos.y = y;
            },
            Input::Move(Motion::MouseRelative(dx, dy)) => {
                if self.mouse_depressed {
                    self.mouse_dragged_tot += dx.abs() + dy.abs();
                    println!("drag");
                    // drag
                }
            },
            Input::Move(Motion::MouseScroll(dx, dy)) => {
                if self.ctrl_depressed {
                    if dy.abs() > 0.08 {
                        if dy > 0.0 {
                            println!("zoom in");
                        } else {
                            println!("zoom out");
                        }
                    }
                } else {
                    if dx.abs() + dy.abs() > 0.12 {
                        println!("scroll");
                    }
                }
            },
            Input::Press(Button::Mouse(mouse::MouseButton::Left)) => {
                self.mouse_depressed = true;
            },
            Input::Release(Button::Mouse(mouse::MouseButton::Left)) => {
                if self.mouse_dragged_tot < 1.5 {
                    println!("click");
                    // click
                }
                self.mouse_dragged_tot = 0.0;
                self.mouse_depressed = false;
            },
            Input::Press(Button::Keyboard(key)) => match key {
                keyboard::Key::LCtrl | keyboard::Key::RCtrl => {
                    self.ctrl_depressed = true;
                },
                keyboard::Key::NumPadPlus | keyboard::Key::Plus | keyboard::Key::Equals => {
                    if self.ctrl_depressed {
                        println!("zoom in");
                        // zoom in (fixed interval)
                    }
                },
                keyboard::Key::NumPadMinus | keyboard::Key::Minus | keyboard::Key::Underscore => {
                    if self.ctrl_depressed {
                        println!("zoom out");
                        // zoom out (fixed interval)
                    }
                },
                _ => {}
            },
            Input::Release(Button::Keyboard(key)) => match key {
                keyboard::Key::LCtrl | keyboard::Key::RCtrl => {
                    self.ctrl_depressed = false;
                },
                _ => {}
            },
            _ => {}
        }
    }
}

fn main() {
    let mut window: PistonWindow =
        WindowSettings::new("Hello World!", [500, 400]).build().unwrap();
    window.events.set_ups(1);

    let mut input_manager = InputManager::new();

    while let Some(ref evt) = window.next() {
        match *evt {
            Event::Input(ref input) => input_manager.handle_input_event(input),
            Event::Update(UpdateArgs { dt }) => {
                // println!("UPDATE");
            },
            _ => {}
        }

        window.draw_2d(evt, |c, g| {
            clear([0.5, 0.5, 0.5, 1.0], g);
            rectangle([1.0, 0.0, 0.0, 1.0], // red
                      [0.0, 0.0,
                       input_manager.mouse_pos.x,
                       input_manager.mouse_pos.y], // rectangle
                      c.transform, g);
        });
    }
}
