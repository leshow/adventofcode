use std::collections::{HashSet, VecDeque};

const DIRS: [(isize, isize); 4] = [(1, 0), (0, 1), (-1, 0), (0, -1)];

pub(crate) fn run() {
    let input = include_str!("../input/day_18.txt");
    // dbg!(path(input));
    println!("{}", pathfinder_part1(input));
}

fn path(input: &str) -> i32 {
    let grid: Vec<Vec<char>> = input.lines().map(|line| line.chars().collect()).collect();

    let (x, y, _) = grid
        .iter()
        .enumerate()
        .flat_map(|(ri, row)| row.iter().enumerate().map(move |(ci, c)| (ri, ci, c)))
        .find(|x| x.2 == &'@')
        .unwrap();

    let doors: HashSet<char> = input
        .chars()
        .filter(|x| x.is_alphabetic())
        .map(|x| x.to_ascii_uppercase())
        .collect();

    let start: (i32, i32, Vec<char>, i32) = (x as i32, y as i32, Vec::new(), 0);

    let mut q = VecDeque::new();
    q.push_back(start);

    while let Some((x, y, keys, dist)) = q.pop_front() {
        if keys.len() == doors.len() {
            return dist;
        }
        for (dx, dy) in &DIRS {
            let x = x + *dx as i32;
            let y = y + *dy as i32;
            let target = grid[x as usize][y as usize];
            if target == '#' {
                continue;
            }
            if target.is_alphabetic()
                && target.is_ascii_uppercase()
                && !keys.contains(&target.to_ascii_lowercase())
            {
                continue;
            }
            let mut keys = keys.clone();
            if target.is_alphabetic() && target.is_ascii_lowercase() {
                keys.push(target);
                keys.sort();
                keys.dedup();
            }
            q.push_back((x, y, keys, dist + 1));
        }
    }
    0
}

fn pathfinder_part1(input: &str) -> i32 {
    let grid: Vec<Vec<char>> = input.lines().map(|line| line.chars().collect()).collect();

    let (x, y, _) = grid
        .iter()
        .enumerate()
        .flat_map(|(i, row)| row.iter().enumerate().map(move |(j, c)| (i, j, c)))
        .find(|x| x.2 == &'@')
        .unwrap();

    let doors: HashSet<char> = input
        .chars()
        .filter(|x| x.is_alphabetic())
        .map(|x| x.to_ascii_uppercase())
        .collect();

    let start: (i32, i32, Vec<char>) = (x as i32, y as i32, Vec::new());
    let result = pathfinding::directed::bfs::bfs(
        &start,
        |(x, y, keys)| {
            let mut moves = Vec::new();
            DIRS.iter().for_each(|&(dx, dy)| {
                do_move(&mut moves, &grid, x + dx as i32, y + dy as i32, &keys)
            });
            moves
        },
        |(_, _, keys)| keys.len() == doors.len(),
    );

    // let result = result.unwrap();

    // for row in result.iter() {
    //     println!("{:?}", row);
    // }

    result.unwrap().len() as i32 - 1
}

fn do_move(
    moves: &mut Vec<(i32, i32, Vec<char>)>,
    grid: &[Vec<char>],
    x: i32,
    y: i32,
    keys: &[char],
) {
    let node = grid[x as usize][y as usize];
    if node == '#'
        || (node.is_alphabetic()
            && node.is_ascii_uppercase()
            && !keys.contains(&node.to_ascii_lowercase()))
    {
        return;
    }

    let mut keys = keys.to_vec();
    if node.is_alphabetic() && node.is_ascii_lowercase() {
        keys.push(node);
        keys.sort();
        keys.dedup();
    }
    moves.push((x, y, keys))
}
