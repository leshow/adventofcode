use std::collections::{HashMap, HashSet, VecDeque};

const DIRS: [(isize, isize); 4] = [(1, 0), (0, 1), (-1, 0), (0, -1)];

pub(crate) fn run() {
    let input = include_str!("../input/day_18_test.txt");
    let grid = input
        .lines()
        .map(|line| line.chars().collect::<Vec<_>>())
        .collect::<Vec<_>>();
    count_steps(&grid);
}

fn count_steps(grid: &[Vec<char>]) {
    let mut routeinfo = HashMap::new();
    for y in 0..grid.len() {
        for x in 0..grid[y].len() {
            let node = grid[y][x];
            if valid_key(node) {
                routeinfo
                    .entry(node)
                    .or_insert_with(|| find_nearest(&grid, y, x));
            }
        }
    }
    // let cur = '@';

    // for reachable in routeinfo[cur] {
    // }
    unimplemented!();
}

fn find_nearest(grid: &[Vec<char>], i: usize, j: usize) -> HashMap<char, (usize, Vec<char>)> {
    let mut q = VecDeque::new();
    q.push_back((i, j, 0, vec![]));
    let mut routeinfo = HashMap::new();
    let mut visited = HashSet::new();
    visited.insert((i, j));

    while let Some((x, y, dist, mut route)) = q.pop_front() {
        let node = grid[x][y];
        if !valid_char(node) && dist > 0 {
            routeinfo.entry(node).or_insert((dist, route.clone()));
            route.push(node);
        }
        visited.insert((x, y));
        for dir in &DIRS {
            let (nx, ny) = ((x as isize + dir.0) as usize, (y as isize + dir.1) as usize);
            if grid[nx][ny] != '#'
                && !visited.contains(&(nx, ny))
                && nx < grid.len()
                && ny < grid[nx].len()
            {
                q.push_back((nx, ny, dist + 1, route.clone()));
            }
        }
    }
    routeinfo
}

fn valid_char(c: char) -> bool {
    match c {
        '.' | '@' | '#' => true,
        _ => false,
    }
}
fn valid_key(c: char) -> bool {
    match c {
        '@' | 'a' | 'b' | 'c' | 'd' | 'e' | 'f' | 'g' | 'h' | 'i' | 'j' | 'k' | 'l' | 'm' | 'n'
        | 'o' | 'p' | 'q' | 'r' | 's' | 't' | 'u' | 'v' | 'w' | 'x' | 'y' | 'z' => true,
        _ => false,
    }
}
