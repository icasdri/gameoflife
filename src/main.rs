/*
 * Copyright 2017 icasdri
 *
 * This file is part of gameoflife. The original source code for gameoflife can
 * be found at <https://github.com/icasdri/gameoflife>. See COPYING for
 * licensing details.
 */

extern crate piston_window;

use piston_window::*;

#[derive(Clone, Copy, Debug)]
struct Point {
    x: f64,
    y: f64,
}

impl Point {
    fn map<F: Fn(f64) -> f64>(&self, f: F) -> Self {
        Point {
            x: f(self.x),
            y: f(self.y),
        }
    }
}

#[derive(Clone, Copy, Debug)]
struct Coord {
    x: usize,
    y: usize,
}

impl From<Point> for Coord {
    fn from(p: Point) -> Self {
        Coord {
            x: p.x as usize,
            y: p.y as usize,
        }
    }
}

#[derive(Clone, Copy, Debug)]
struct Geo {
    w: f64,
    h: f64,
}

impl From<Point> for Geo {
    fn from(p: Point) -> Self {
        Geo {
            w: p.x,
            h: p.y,
        }
    }
}

impl std::fmt::Display for Point {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "({}, {})", self.x, self.y)
    }
}

impl std::fmt::Display for Coord {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "({}, {})", self.x, self.y)
    }
}

impl std::fmt::Display for Geo {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "[{} x {}]", self.w, self.h)
    }
}

#[derive(Clone, Copy, Debug)]
struct WinGeo {
    w: u32,
    h: u32
}

#[derive(Clone, Copy, Debug)]
struct Mvmt {
    dx: f64,
    dy: f64,
}

type ZoomFactor = f64;
type SpeedDiff = i32;

#[derive(Debug)]
enum ManagedInputEvent {
    Click(Point),
    Drag(Mvmt),
    Scroll(Mvmt),
    Zoom(ZoomFactor),
    PauseToggle,
    Speed(SpeedDiff),
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
                    return Some(MIE::Drag(Mvmt { dx: dx, dy: dy }));
                }
            },
            Input::Move(Motion::MouseScroll(dx, dy)) => {
                if self.ctrl_depressed {
                    if dy.abs() > 0.08 {
                        if dy > 0.0 {
                            // zoom in
                            return Some(MIE::Zoom(dy));
                        } else {
                            // soom out
                            return Some(MIE::Zoom(dy));
                        }
                    }
                } else {
                    if dx.abs() + dy.abs() > 0.12 {
                        // scroll
                        return Some(MIE::Scroll(Mvmt { dx: 3.0 * dx, dy: 3.0 * dy }));
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
                    return Some(MIE::Click(Point { x: self.mouse_pos.x, y: self.mouse_pos.y }));
                }
            },
            Input::Press(Button::Keyboard(key)) => match key {
                keyboard::Key::LCtrl | keyboard::Key::RCtrl => {
                    self.ctrl_depressed = true;
                },
                keyboard::Key::NumPadPlus | keyboard::Key::Plus | keyboard::Key::Equals => {
                    if self.ctrl_depressed {
                        // zoom in
                        return Some(MIE::Zoom(1.5));
                    }
                },
                keyboard::Key::NumPadMinus | keyboard::Key::Minus | keyboard::Key::Underscore => {
                    if self.ctrl_depressed {
                        // zoom out
                        return Some(MIE::Zoom(-1.5));
                    }
                },
                keyboard::Key::M => {
                    return Some(MIE::Zoom(1.5));
                },
                keyboard::Key::N => {
                    return Some(MIE::Zoom(-1.5));
                },
                keyboard::Key::Space | keyboard::Key::P => {
                    return Some(MIE::PauseToggle);
                },
                keyboard::Key::LeftBracket | keyboard::Key::U => {
                    return Some(MIE::Speed(-1));
                },
                keyboard::Key::RightBracket | keyboard::Key::I => {
                    return Some(MIE::Speed(1));
                },
                keyboard::Key::Up | keyboard::Key::W | keyboard::Key::K => {
                    return Some(MIE::Scroll(Mvmt { dx: 0.0, dy: 6.0 }));
                },
                keyboard::Key::Down | keyboard::Key::S | keyboard::Key::J => {
                    return Some(MIE::Scroll(Mvmt { dx: 0.0, dy: -6.0 }));
                },
                keyboard::Key::Left | keyboard::Key::A | keyboard::Key::H => {
                    return Some(MIE::Scroll(Mvmt { dx: 6.0, dy: 0.0 }));
                },
                keyboard::Key::Right | keyboard::Key::D | keyboard::Key::L => {
                    return Some(MIE::Scroll(Mvmt { dx: -6.0, dy: 0.0 }));
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


static BASE_COLOR: [f32; 4] = [0.5, 0.5, 0.5, 1.0];
static ALIVE_COLOR: [f32; 4] = [0.9, 0.9, 0.35, 1.0];
static OVERLAY_COLOR: [f32; 4] = [1.0, 1.0, 1.0, 0.81];

struct ViewportManager {
    nonce: usize,
    vp_pos: Point,  // world coords
    vp_geo: Geo,  // world coords
    window_geo: WinGeo,
}

struct PredrawData {
    sl: f64,
    head: Geo,
    offset: Point,
    start: Coord,
    end: Coord,
    maybe_mouse: Option<Coord>,
}

impl ViewportManager {
    fn new(world: &World, initial_window: WinGeo) -> Self {
        ViewportManager {
            nonce: 0,
            vp_pos: Point { x: 0.0, y: 0.0 },
            vp_geo: Geo { w: world.size.x as f64, h: world.size.y as f64 },
            window_geo: initial_window,
        }
    }

    fn handle_resize(&mut self, new_w: u32, new_h: u32) {
        self.window_geo.w = new_w;
        self.window_geo.h = new_h;
        self.nonce.wrapping_add(1);
    }

    fn zoom(&mut self, factor: ZoomFactor) {
        self.vp_geo.w -= factor;
        self.vp_geo.h -= factor;
        if self.vp_geo.w < 0.0 {
            self.vp_geo.w = 0.0;
        }
        if self.vp_geo.h < 0.0 {
            self.vp_geo.h = 0.0;
        }
        self.vp_pos.x += factor / 2.0;
        self.vp_pos.y += factor / 2.0;
        self.nonce.wrapping_add(1);
    }

    fn pan(&mut self, m: Mvmt) {
        let sl = self.window_geo.w as f64 / self.vp_geo.w;
        self.vp_pos.x -= m.dx / sl;
        self.vp_pos.y -= m.dy / sl;
        self.nonce.wrapping_add(1);
    }

    fn predraw(&self, w: &World, raw_mouse: &Point) -> PredrawData {
        /*
         * side length of each square in window coords
         * (note that side length in world coords is 1)
         */
        let sl = self.window_geo.w as f64 / self.vp_geo.w;

        let head: Geo = self.vp_pos.map(|f| if f > 0.0 { f.fract() } else { 0.0 }).into();
        let offset: Point = self.vp_pos.map(|f| if f < 0.0 { -f } else { 0.0 });
        let start: Coord = self.vp_pos.map(|f| if f > 0.0 { f.trunc() } else { 0.0 }).into();
        let end = Coord {
            x: std::cmp::min(start.x + 1 + self.vp_geo.w.ceil() as usize, w.size.x),
            y: std::cmp::min(start.y + 1 + self.vp_geo.h.ceil() as usize, w.size.y),
        };

        let mouse_x = {
            let xf = ((raw_mouse.x / sl) - offset.x + head.w + start.x as f64).trunc();
            if xf >= 0.0 {
                let x = xf as usize;
                if x < w.size.x { Some(x) } else { None }
            } else { None }
        };
        let mouse_y = {
            let yf = ((raw_mouse.y / sl) - offset.y + head.h + start.y as f64).trunc();
            if yf >= 0.0 {
                let y = yf as usize;
                if y < w.size.y { Some(y) } else { None }
            } else { None }
        };

        PredrawData {
            sl: sl,
            head: head,
            offset: offset,
            start: start,
            end: end,
            maybe_mouse: if mouse_x.is_some() && mouse_y.is_some() {
                Some(Coord { x: mouse_x.unwrap(), y: mouse_y.unwrap() })
            } else { None },
        }
    }

    fn draw(&self, window: &mut PistonWindow, pd: &PredrawData, w: &World, ldr: &mut LastDrawnNonces, evt: &Input) {
        println!("{} -- {} | {} -- {}", ldr.world_ld_nonce, w.nonce, ldr.vp_ld_nonce, self.nonce);

        let &PredrawData { sl, head, offset, start, end, maybe_mouse } = pd;

        if ldr.world_ld_nonce != w.nonce || ldr.vp_ld_nonce != self.nonce {
            ldr.world_ld_nonce = w.nonce;
            ldr.vp_ld_nonce = self.nonce;

            window.draw_2d(evt, |c, g| {
                clear(BASE_COLOR, g);

                for x in start.x..end.x {
                    for y in start.y..end.y {
                        if w.active[y][x] {
                            rectangle(ALIVE_COLOR,
                                      [((x - start.x) as f64 + offset.x - head.w) * sl,
                                       ((y - start.y) as f64 + offset.y - head.h) * sl,
                                       sl, sl],
                                      c.transform, g);
                        }
                    }
                }
            });

        }

        if let Some(mouse) = maybe_mouse {
            if mouse.x >= start.x && mouse.y >= start.y {
                window.draw_2d(evt, |c, g| {
                    rectangle(OVERLAY_COLOR,
                              [((mouse.x - start.x) as f64 + offset.x - head.w) * sl,
                               ((mouse.y - start.y) as f64 + offset.y - head.h) * sl,
                               sl, sl],
                              c.transform, g);
                });
            }
        }
        
        window.swap_buffers();
    }
}

struct CoordArray<T: Clone> {
    size: Coord,
    v: Vec<T>,
}

impl<T> CoordArray<T> where T: Clone {
    fn new_filled(default: T, size: Coord) -> Self {
        CoordArray {
            size: size,
            v: vec![default; size.x * size.y],
        }
    }
}

impl<T> std::ops::Index<usize> for CoordArray<T> where T: Clone {
    type Output = [T];

    fn index(&self, row: usize) -> &[T] {
        &self.v[row * self.size.x .. (row + 1) * self.size.x]
    }
}

impl<T> std::ops::IndexMut<usize> for CoordArray<T> where T: Clone {
    fn index_mut(&mut self, row: usize) -> &mut [T] {
        &mut self.v[row * self.size.x .. (row + 1) * self.size.x]
    }
}

impl<T> std::ops::Index<Coord> for CoordArray<T> where T: Clone {
    type Output = T;

    fn index(&self, coord: Coord) -> &T {
        &self.v[coord.y * self.size.y + coord.x]
    }
}

impl<T> std::ops::IndexMut<Coord> for CoordArray<T> where T: Clone {
    fn index_mut(&mut self, coord: Coord) -> &mut T {
        &mut self.v[coord.y * self.size.y + coord.x]
    }
}

impl std::fmt::Display for CoordArray<bool> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        for i in 0..self.size.y {
            let s = &self[i];
            for c in s {
                try!(write!(f, "{} ", if *c { "■" } else { "□" }));
            }
            try!(writeln!(f, ""));
        }
        Ok(())
    }
}

struct World {
    nonce: usize,
    size: Coord,
    active: CoordArray<bool>,
    staged: CoordArray<bool>,
}

static NEIGHBORS_TRANSLATIONS: [(i32, i32); 8] =
    [(-1, -1), (0, -1), (1, -1),
     (-1,  0),          (1,  0),
     (-1,  1), (0,  1), (1,  1)];

#[derive(Debug)]
enum CheckedCoord {
    Valid(Coord),
    Invalid,
}

#[derive(Clone)]
struct NeighborsIter {
    world_size: Coord,
    next_op: usize,
    orig_coord: Coord,
}

impl NeighborsIter {
    fn new(world_size: Coord, c: Coord) -> Self {
        NeighborsIter {
            world_size: world_size,
            next_op: 0,
            orig_coord: c,
        }
    }
}

impl std::iter::Iterator for NeighborsIter {
    type Item = CheckedCoord;

    fn next(&mut self) -> Option<CheckedCoord> {
        if self.next_op >= NEIGHBORS_TRANSLATIONS.len() {
            return None;
        }

        let (dx, dy) = NEIGHBORS_TRANSLATIONS[self.next_op];

        let x = self.orig_coord.x as i32 + dx;
        let y = self.orig_coord.y as i32 + dy;

        self.next_op += 1;

        if x < 0 || x as usize >= self.world_size.x {
            return Some(CheckedCoord::Invalid);
        }

        if y < 0 || y as usize >= self.world_size.y {
            return Some(CheckedCoord::Invalid);
        }

        Some(CheckedCoord::Valid(Coord { x: x as usize, y: y as usize}))
    }
}

impl std::fmt::Display for NeighborsIter {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        for i in self.clone() {
            try!(writeln!(f, "{:?}", i));
        }
        Ok(())
    }
}

impl World {
    fn new(size: Coord) -> Self {
        World {
            nonce: 0,
            size: size,
            active: CoordArray::new_filled(false, size),
            staged: CoordArray::new_filled(false, size),
        }
    }

    fn neighbors(&self, c: Coord) -> NeighborsIter {
        NeighborsIter::new(self.size, c)
    }

    fn num_neighbors(&self, c: Coord) -> usize {
        self.neighbors(c).map(|n| match n {
            CheckedCoord::Valid(c) if self.active[c] => 1,
            _ => 0
        }).sum()
    }

    fn step_coord(&mut self, c: Coord) {
        self.staged[c] = match (self.num_neighbors(c), self.active[c]) {
            (2 ... 3, true) => true,
            (3, false) => true,
            _ => false,
        }
    }

    fn step(&mut self) {
        for x in 0..self.size.x {
            for y in 0..self.size.y {
                self.step_coord(Coord{ x: x, y: y});
            }
        }
    }

    fn swap_staged(&mut self) {
        std::mem::swap(&mut self.active, &mut self.staged);
        self.nonce.wrapping_add(1);
    }
}

struct LastDrawnNonces {
    world_ld_nonce: usize,
    vp_ld_nonce: usize,
}

impl LastDrawnNonces {
    fn new() -> Self {
        LastDrawnNonces {
            world_ld_nonce: 100,  // some non-zero starting value
            vp_ld_nonce: 100,  // some non-zero starting value
        }
    }
}

static WIDTH: u32 = 950;
static HEIGHT: u32 = 700;

fn update_title(window: &mut PistonWindow, paused: bool, speed: u64) {
    window.set_title(format!(
        "{}Conway's Game of Life (speed: {} Hz)",
        if paused { "(PAUSED) " } else { "" },
        speed
    ));
}

fn main() {
    let mut world = World::new(Coord { x: 150, y: 150 });

    let mut window: PistonWindow =
        WindowSettings::new("Conway's Game of Life", [WIDTH, HEIGHT]).build().unwrap();
    window.events.set_ups(1);
    window.set_swap_buffers(false);

    let mut im = InputManager::new();
    let mut vp = ViewportManager::new(&world, WinGeo { w: WIDTH, h: HEIGHT });

    let mut paused = false;
    let mut speed = 1;

    update_title(&mut window, paused, speed);

    let mut ldr = LastDrawnNonces::new();

    while let Some(ref evt) = window.next() {
        let pd = vp.predraw(&world, &im.mouse_pos);

        match *evt {
            Input::Resize(new_w, new_h) => vp.handle_resize(new_w, new_h),
            Input::Update(_) => {
                if !paused {
                    world.step();
                    world.swap_staged();
                }
            },
            _ => if let Some(mie) = im.handle_input_event(&evt) {
                match mie {
                    MIE::Click(_) => {
                        if let Some(mouse) = pd.maybe_mouse {
                            let mut c = &mut world.active[mouse];
                            *c = !*c;
                        }
                    },
                    MIE::Drag(m) | MIE::Scroll(m) => {
                        vp.pan(m);
                    },
                    MIE::Zoom(f) => {
                        vp.zoom(f);
                    },
                    MIE::PauseToggle => {
                        if paused {
                            paused = false;
                            window.events.set_ups(speed);
                        } else {
                            paused = true;
                            window.events.set_ups(1);
                        }
                        update_title(&mut window, paused, speed);
                    },
                    MIE::Speed(diff) => {
                        let s = speed as i32 + diff;
                        if s > 0 {
                            speed = s as u64;
                            window.events.set_ups(speed);
                            update_title(&mut window, paused, speed);
                        }
                    }
                }
            }
        }

        vp.draw(&mut window, &pd, &world, &mut ldr, evt);
    }
}
