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

#[derive(Clone, Copy, Debug)]
struct GeoInt {
    w: u32,
    h: u32
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

#[derive(Debug)]
enum ManagedInputEvent {
    Click { x: f64, y: f64 },
    Drag { dx: f64, dy: f64 },
    Scroll { dx: f64, dy: f64 },
    Zoom { df: f64 },
}
use ManagedInputEvent as MIE;

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

    fn handle_input_event(&mut self, input: &Input) -> Option<ManagedInputEvent> {
        match *input {
            Input::Move(Motion::MouseCursor(x, y)) => {
                self.mouse_pos.x = x;
                self.mouse_pos.y = y;
            },
            Input::Move(Motion::MouseRelative(dx, dy)) => {
                if self.mouse_depressed {
                    self.mouse_dragged_tot += dx.abs() + dy.abs();
                    // drag
                    return Some(MIE::Drag { dx: dx, dy: dy })
                }
            },
            Input::Move(Motion::MouseScroll(dx, dy)) => {
                if self.ctrl_depressed {
                    if dy.abs() > 0.08 {
                        if dy > 0.0 {
                            // zoom in
                            return Some(MIE::Zoom { df: dy })
                        } else {
                            // soom out
                            return Some(MIE::Zoom { df: dy })
                        }
                    }
                } else {
                    if dx.abs() + dy.abs() > 0.12 {
                        // scroll
                        return Some(MIE::Scroll { dx: dx, dy: dy })
                    }
                }
            },
            Input::Press(Button::Mouse(mouse::MouseButton::Left)) => {
                self.mouse_depressed = true;
            },
            Input::Release(Button::Mouse(mouse::MouseButton::Left)) => {
                let last_dragged = self.mouse_dragged_tot;
                self.mouse_dragged_tot = 0.0;
                self.mouse_depressed = false;
                if last_dragged < 1.5 {
                    // click
                    return Some(MIE::Click { x: self.mouse_pos.x, y: self.mouse_pos.y })
                }
            },
            Input::Press(Button::Keyboard(key)) => match key {
                keyboard::Key::LCtrl | keyboard::Key::RCtrl => {
                    self.ctrl_depressed = true;
                },
                keyboard::Key::NumPadPlus | keyboard::Key::Plus | keyboard::Key::Equals => {
                    if self.ctrl_depressed {
                        // zoom in
                        return Some(MIE::Zoom { df: 1.5 });
                    }
                },
                keyboard::Key::NumPadMinus | keyboard::Key::Minus | keyboard::Key::Underscore => {
                    if self.ctrl_depressed {
                        // zoom out
                        return Some(MIE::Zoom { df: -1.5 });
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
        return None;
    }
}

struct ViewportManager {
    vp_pos: Point,  // world coords
    vp_geo: Geo,  // world coords
    window_geo: GeoInt,
}

impl ViewportManager {
    fn new(initial_window: GeoInt) -> Self {
        ViewportManager {
            vp_pos: Point { x: 0.0, y: 0.0 },
            vp_geo: Geo { w: initial_window.w as f64, h: initial_window.h as f64 },
            window_geo: initial_window,
        }
    }

    fn handle_resize(&mut self, new_w: u32, new_h: u32) {
        self.window_geo.w = new_w;
        self.window_geo.h = new_h;
        println!("resized");
    }

}

enum ConwayCell {
    Alive,
    NotAlive,
}

struct World {
    arr: Vec<ConwayCell>,
}

static WIDTH: u32 = 500;
static HEIGHT: u32 = 400;

fn main() {
    let mut window: PistonWindow =
        WindowSettings::new("Hello World!", [WIDTH, HEIGHT]).build().unwrap();
    window.events.set_ups(1);

    let mut input_manager = InputManager::new();
    let mut vp = ViewportManager::new(GeoInt { w: WIDTH, h: HEIGHT });

    while let Some(ref evt) = window.next() {
        match *evt {
            Event::Input(Input::Resize(new_w, new_h)) => vp.handle_resize(new_w, new_h),
            Event::Input(ref input) => {
                if let Some(mie) = input_manager.handle_input_event(input) {
                    println!("{:?}", mie);
                }
            }
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
            rectangle([0.0, 0.9, 0.0, 1.0], // green
                      [5.0, 5.0, 20.0, 20.0], c.transform, g);
        });
    }
}
