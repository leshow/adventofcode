fn build_layers(w: usize, h: usize) -> Vec<Vec<char>> {
    let input = include_str!("../input/day8.txt");
    let len = w * h;
    let mut iter = input.chars();

    (0..)
        .map(|_| iter.by_ref().take(len).collect::<Vec<char>>())
        .take_while(|s| !s.is_empty() && s.len() == len)
        .collect::<Vec<_>>()
}

pub fn run() {
    if let Some(res) = part1() {
        println!("part1 {}", res);
    }
    part2(25, 6);
}

fn part1() -> Option<usize> {
    let layers = build_layers(25, 6);
    let res = layers
        .iter()
        .enumerate()
        .map(|(i, layer)| (i, count_num(&layer[..], 0)))
        .min_by(|a, b| a.1.cmp(&b.1));

    res.map(|(min_idx, _)| {
        let ones = count_num(&layers[min_idx], 1);
        let twos = count_num(&layers[min_idx], 2);
        ones * twos
    })
}

fn part2(w: usize, h: usize) {
    let layers = build_layers(w, h);
    (0..h).for_each(|i| {
        let line = (0..w)
            .map(|j| get_pixel(&layers, i, j, w, h))
            .collect::<String>();
        println!("{}", line);
    })
}

fn count_num(layer: &[char], num: u32) -> usize {
    layer
        .iter()
        .filter(|n| n.to_digit(10).unwrap() == num)
        .count()
}

fn get_pixel(layers: &[Vec<char>], i: usize, j: usize, w: usize, h: usize) -> char {
    for layer in &layers[..] {
        match layer[i * w + j].to_digit(10).unwrap() {
            2 => continue,
            1 => return '#',
            _ => return ' ',
        }
    }
    ' '
}
