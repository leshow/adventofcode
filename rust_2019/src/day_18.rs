use std::collections::HashSet;

const DIRS: [(isize, isize); 4] = [(1, 0), (0, 1), (-1, 0), (0, -1)];

pub(crate) fn run() {
    let input = include_str!("../input/day_18_test.txt");
    println!("{}", pathfinder_part1(input));
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
            DIRS.iter()
                .filter_map(|&(dx, dy)| do_move(&grid, x + dx as i32, y + dy as i32, &keys))
                .collect::<Vec<_>>()
        },
        |(_, _, keys)| keys.len() == doors.len(),
    );

    result.unwrap().len() as i32 - 1
}

fn do_move(grid: &[Vec<char>], x: i32, y: i32, keys: &[char]) -> Option<(i32, i32, Vec<char>)> {
    let node = grid[x as usize][y as usize];
    if node == '#'
        || (node.is_alphabetic()
            && node.is_ascii_uppercase()
            && !keys.contains(&node.to_ascii_lowercase()))
    {
        return None;
    }

    let mut keys = keys.to_vec();
    if node.is_alphabetic() && node.is_ascii_lowercase() {
        keys.push(node);
        keys.sort();
        keys.dedup();
    }
    Some((x, y, keys))
}
