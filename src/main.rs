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

impl Coord {
    fn is_within(&self, size: Coord) -> bool {
        self.x < size.x && self.y < size.y
    }

    fn map<F: Fn(usize) -> usize>(&self, f: F) -> Self {
        Coord {
            x: f(self.x),
            y: f(self.y),
        }
    }
}

impl From<Point> for Coord {
    fn from(p: Point) -> Self {
        Coord {
            x: p.x as usize,
            y: p.y as usize,
        }
    }
}

impl From<Geo> for Coord {
    fn from(g: Geo) -> Self {
        Coord {
            x: g.w as usize,
            y: g.h as usize,
        }
    }
}

#[derive(Clone, Copy, Debug)]
struct Mvmt {
    dx: f64,
    dy: f64,
}

type ZoomFactor = f64;

#[derive(Clone, Copy, Debug)]
struct Geo {
    w: f64,
    h: f64,
}

impl Geo {
    fn map<F: Fn(f64) -> f64>(&self, f: F) -> Self {
        Geo {
            w: f(self.w),
            h: f(self.h),
        }
    }
}

impl From<Point> for Geo {
    fn from(p: Point) -> Self {
        Geo {
            w: p.x,
            h: p.y,
        }
    }
}

#[derive(Clone, Copy, Debug)]
struct WinGeo {
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

#[derive(Debug)]
enum ManagedInputEvent {
    Click(Point),
    Drag(Mvmt),
    Scroll(Mvmt),
    Zoom(ZoomFactor),
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
    world_size: Coord,
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
            world_size: world.size,
            vp_pos: Point { x: 0.0, y: 0.0 },
            vp_geo: Geo { w: world.size.x as f64, h: world.size.y as f64 },
            window_geo: initial_window,
        }
    }

    fn handle_resize(&mut self, new_w: u32, new_h: u32) {
        self.window_geo.w = new_w;
        self.window_geo.h = new_h;
        println!("resized");
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
    }

    fn pan(&mut self, m: Mvmt) {
        let sl = self.window_geo.w as f64 / self.vp_geo.w;
        self.vp_pos.x -= m.dx / sl;
        self.vp_pos.y -= m.dy / sl;
        println!("{:?}", self.vp_pos);
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

    /**
     * Draws world based on current view port.
     * Also picks mouse based on given point, draws overlay on mouse-picked
     * tile, and returns world coordinate of tile picked.
     */
    fn draw(&self, pd: &PredrawData, w: &World, c: &Context, g: &mut G2d) {
        let &PredrawData { sl, head, offset, start, end, maybe_mouse } = pd;

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

        if let Some(mouse) = maybe_mouse {
            rectangle(OVERLAY_COLOR,
                      [((mouse.x - start.x) as f64 + offset.x - head.w) * sl,
                       ((mouse.y - start.y) as f64 + offset.y - head.h) * sl,
                       sl, sl],
                      c.transform, g);
        }
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
    size: Coord,
    active: CoordArray<bool>,
    staged: CoordArray<bool>,
}

#[derive(Clone, Copy)]
enum UnitTranslationOp {
    ADD, SUB, NOP,
}
use UnitTranslationOp as U;

trait ApplyOp<O> {
    fn apply_op(&self, op: O) -> Result<Self, ()> where Self: Sized;
}

impl ApplyOp<UnitTranslationOp> for usize {
    fn apply_op(&self, op: UnitTranslationOp) -> Result<Self, ()> {
        match op {
            U::SUB => self.checked_sub(1).ok_or(()),
            U::ADD => Ok(*self + 1),
            U::NOP => Ok(*self)
        }
    }
}

impl ApplyOp<(UnitTranslationOp, UnitTranslationOp)> for Coord {
    fn apply_op(&self, op: (UnitTranslationOp, UnitTranslationOp)) -> Result<Self, ()> {
        Ok(Coord {
            x: try!(self.x.apply_op(op.0)),
            y: try!(self.y.apply_op(op.1)),
        })
    }
}

static NEIGHBORS_TRANSLATIONS: [(UnitTranslationOp, UnitTranslationOp); 8] =
    [(U::SUB, U::SUB), (U::NOP, U::SUB), (U::ADD, U::SUB),
     (U::SUB, U::NOP),                   (U::ADD, U::NOP),
     (U::SUB, U::ADD), (U::NOP, U::ADD), (U::ADD, U::ADD)];

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

        let mut ret = Some(CheckedCoord::Invalid);

        if let Ok(next_coord) = self.orig_coord.apply_op(NEIGHBORS_TRANSLATIONS[self.next_op]) {
            if next_coord.is_within(self.world_size) {
                ret = Some(CheckedCoord::Valid(next_coord));
            }
        }

        self.next_op += 1;
        ret
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
            size: size,
            active: CoordArray::new_filled(false, size),
            staged: CoordArray::new_filled(false, size),
        }
    }

    fn new_for_testing() -> Self {
        let mut w = World::new(Coord { x: 35, y: 35 });
        w.active[0][0] = true;
        w.active[0][1] = true;
        w.active[1][0] = true;
        w.active[8][34] = true;
        w.active[10][10] = true;
        w.active[10][11] = true;
        w.active[9][12] = true;
        w.active[11][12] = true;
        w.active[10][13] = true;
        w.active[10][14] = true;
        w.active[10][15] = true;
        w.active[10][16] = true;
        w.active[9][17] = true;
        w.active[11][17] = true;
        w.active[10][18] = true;
        w.active[10][19] = true;
        w
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
    }
}

static WIDTH: u32 = 500;
static HEIGHT: u32 = 400;

fn main() {
    let mut world = World::new_for_testing();

    println!("{}", world.neighbors(Coord { x: 0, y: 0}));
    println!("{}", world.num_neighbors(Coord { x: 0, y: 0}));
    println!("---");
    println!("{}", world.neighbors(Coord { x: 11, y: 12}));
    println!("{}", world.num_neighbors(Coord { x: 11, y: 12}));

    println!("{}", world.active);

    let mut window: PistonWindow =
        WindowSettings::new("Hello World!", [WIDTH, HEIGHT]).build().unwrap();
    window.events.set_ups(1);

    let mut im = InputManager::new();
    let mut vp = ViewportManager::new(&world, WinGeo { w: WIDTH, h: HEIGHT });

    while let Some(ref evt) = window.next() {
        let pd = vp.predraw(&world, &im.mouse_pos);

        match *evt {
            Event::Input(Input::Resize(new_w, new_h)) => vp.handle_resize(new_w, new_h),
            Event::Input(ref input) => {
                if let Some(mie) = im.handle_input_event(input) {
                    println!("{:?}", mie);
                    match mie {
                        MIE::Click(p) => {
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
                        }
                    }
                }
            }
            Event::Update(UpdateArgs { dt }) => {
                world.step();
                world.swap_staged();
                println!("{}", world.active);
            },
            _ => {}
        }

        window.draw_2d(evt, |c, g| {
            clear(BASE_COLOR, g);
            vp.draw(&pd, &world, &c, g);
        });
    }
}
