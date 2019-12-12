use self::Space::*;
use std::{
    collections::HashSet,
    ops::{Add, Sub},
};

pub(crate) fn run() {
    let input = include_str!("../input/day9.txt");
    let asteroids = input
        .lines()
        .enumerate()
        .flat_map(|(y, line)| {
            line.chars()
                .enumerate()
                .map(|(x, c)| (x, Space::from(c)))
                .filter(|(_, s)| *s == Asteroid)
                .map(move |(x, _)| Point::new(x, y))
        })
        .collect::<Vec<Point>>();
    let ans = part1(&asteroids);
    println!("{:?}", ans);
}

fn part1(asteroids: &[Point]) -> Option<(Point, usize)> {
    let mut counts = asteroids
        .iter()
        .map(|ast| {
            let mut angles = asteroids
                .iter()
                .filter(|&it| it != ast)
                .map(|it| it.angle(ast))
                .collect::<Vec<_>>();
            angles.sort_by(|a, b| a.partial_cmp(b).unwrap());
            let mut uniq = 0;
            println!("{:?}", angles);
            for i in 0..(angles.len() - 1) {
                if angles[i] != angles[i + 1] {
                    uniq += 1;
                }
            }
            (*ast, uniq)
        })
        .collect::<Vec<_>>();

    counts.sort_by(|a, b| a.1.cmp(&b.1));
    counts.pop()
}

#[derive(Debug, Eq, Copy, Ord, PartialEq, PartialOrd, Clone)]
enum Space {
    Asteroid,
    Empty,
}

#[derive(Debug, Eq, Copy, Ord, PartialEq, PartialOrd, Clone)]
struct Point {
    x: isize,
    y: isize,
}

impl Add<Point> for Point {
    type Output = Point;

    fn add(self, other: Point) -> Self::Output {
        Point {
            x: self.x + other.x,
            y: self.y + other.y,
        }
    }
}

impl Sub<Point> for Point {
    type Output = Point;

    fn sub(self, other: Point) -> Self::Output {
        Point {
            x: self.x - other.x,
            y: self.y - other.y,
        }
    }
}

impl Point {
    pub(crate) fn new(x: usize, y: usize) -> Self {
        Point {
            x: x as isize,
            y: y as isize,
        }
    }

    pub(crate) fn angle(&self, other: &Point) -> FloatCmp {
        let sub = *self - *other;
        FloatCmp((sub.y as f32).atan2(sub.x as f32))
    }
}

#[derive(Copy, Debug, Clone, PartialOrd)]
struct FloatCmp(f32);

impl PartialEq for FloatCmp {
    fn eq(&self, other: &FloatCmp) -> bool {
        (self.0 - other.0).abs() < 0.00001
    }
}
impl Eq for FloatCmp {}

impl From<char> for Space {
    fn from(c: char) -> Self {
        match c {
            '#' => Asteroid,
            '.' => Empty,
            _ => unreachable!(),
        }
    }
}
