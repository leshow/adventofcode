use self::Dir::*;
use std::{
    collections::{HashMap, HashSet},
    fs::File,
    io::{self, prelude::*, BufReader},
};

pub fn run() -> io::Result<()> {
    let rdr = BufReader::new(File::open("./input/day3.txt")?);
    let moves = rdr
        .lines()
        .map(|line| line.unwrap().split(',').map(Move::new).collect::<Vec<_>>())
        .collect::<Vec<_>>();
    let a = wire_map(&moves[0]);
    let b = wire_map(&moves[1]);

    let mut keys = intersection(&a, &b);
    keys.sort_by(|x, y| manhattan(*x).cmp(&manhattan(*y)));

    println!("Part 1: {:?} {:?}", manhattan(keys[0]), keys[0]);

    let mut lens = keys
        .iter()
        .map(|x| (x, a[x]))
        .zip(keys.iter().map(|x| (x, b[x])))
        .map(|(a, b)| (a.0, a.1 + b.1))
        .collect::<Vec<_>>();
    lens.sort_by(|x, y| x.1.cmp(&y.1));

    println!("Part 2: {:?}", lens[0].1);
    Ok(())
}

fn wire_map(wires: &[Move]) -> HashMap<(i32, i32), usize> {
    let mut x = 0;
    let mut y = 0;
    let mut len = 0;
    let mut points = HashMap::new();
    for &Move(d, i) in wires {
        for _ in 0..i {
            y += d.y();
            x += d.x();
            len += 1;
            points.entry((x, y)).or_insert(len);
        }
    }
    points
}

fn manhattan(x: (i32, i32)) -> i32 {
    x.0.abs() + x.1.abs()
}

fn intersection(a: &HashMap<(i32, i32), usize>, b: &HashMap<(i32, i32), usize>) -> Vec<(i32, i32)> {
    let aset = a.keys().collect::<HashSet<_>>();
    let bset = b.keys().collect::<HashSet<_>>();
    aset.intersection(&bset).map(|x| **x).collect()
}

#[derive(PartialEq, Eq, Debug, Copy, Clone)]
enum Dir {
    L,
    R,
    U,
    D,
}

impl Dir {
    pub fn x(self) -> i32 {
        match self {
            L => -1,
            R => 1,
            U => 0,
            D => 0,
        }
    }

    pub fn y(self) -> i32 {
        match self {
            L => 0,
            R => 0,
            U => 1,
            D => -1,
        }
    }
}

impl From<char> for Dir {
    fn from(c: char) -> Self {
        match c {
            'L' => L,
            'R' => R,
            'D' => D,
            'U' => U,
            _ => panic!("Direction unsupported"),
        }
    }
}

#[derive(PartialEq, Eq, Debug, Copy, Clone)]
struct Move(Dir, usize);

impl Move {
    pub fn new(s: &str) -> Self {
        let num = s[1..].parse::<usize>().expect("failed to parse num");
        Move(s.chars().next().unwrap().into(), num)
    }
}
