use std::cmp::Ordering;

pub fn run(lower: usize, upper: usize) {
    // naively generate nums
    let num = (lower..=upper)
        .map(num_to_vec)
        .filter(|v| never_decreases_adj(v))
        .count();

    let part2 = (lower..=upper)
        .map(num_to_vec)
        .filter(|v| never_decreases_adj(v))
        .filter(|v| group_by(v).any(|group| group.len() == 2))
        .count();

    println!("part1 {}", num);
    println!("part2 {}", part2);
}

fn num_to_vec(num: usize) -> Vec<usize> {
    let mut num = num;
    let mut ret = Vec::new();
    while num > 0 {
        ret.push(num % 10);
        num /= 10;
    }
    ret.reverse();
    ret
}

fn never_decreases_adj(num: &[usize]) -> bool {
    let mut has_adj = false;
    for i in 1..num.len() {
        match num[i - 1].cmp(&num[i]) {
            Ordering::Greater => return false,
            Ordering::Equal => {
                has_adj = true;
            }
            _ => continue,
        }
    }
    has_adj
}

fn group_by<T: Ord>(nums: &'_ [T]) -> impl Iterator<Item = &'_ [T]> {
    let mut start = 0;
    (1..=nums.len()).flat_map(move |i| {
        if i == nums.len() || nums[i - 1] != nums[i] {
            let s = start;
            start = i;
            Some(&nums[s..i])
        } else {
            None
        }
    })
}
