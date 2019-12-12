use self::Space::*;
use ordered_float::OrderedFloat;
use std::{
    cmp::Reverse,
    collections::{BinaryHeap, HashSet},
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
    println!("{:?}", part1(&asteroids));
}

fn part1(asteroids: &[Point]) -> Option<usize> {
    let mut counts = asteroids
        .iter()
        .map(|ast| {
            let count = asteroids
                .iter()
                .filter(|&it| it != ast)
                .map(|it| it.angle(ast))
                .collect::<HashSet<_>>();
            count.len()
        })
        .collect::<BinaryHeap<_>>();

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

    pub(crate) fn angle(&self, other: &Point) -> OrderedFloat<f32> {
        let sub = *self - *other;
        OrderedFloat((sub.y as f32).atan2(sub.x as f32))
    }
}

impl From<char> for Space {
    fn from(c: char) -> Self {
        match c {
            '#' => Asteroid,
            '.' => Empty,
            _ => unreachable!(),
        }
    }
}
