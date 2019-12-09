fn phase(arr: &mut Vec<usize>, start: usize, settings: &mut Vec<Vec<usize>>) {
    if start > 4 {
        settings.push(arr.clone());
        return;
    }
    for i in 0..=4 {
        if !arr.contains(&i) {
            arr.push(i);
            phase(arr, start + 1, settings);
            arr.pop();
        }
    }
}

// oopsy, my intcode stuff is in haskell-- UNFINISHED

pub fn run() {
    let mut settings = Vec::new();
    let mut st = Vec::new();
    phase(&mut st, 0, &mut settings);
    println!("{:?}", settings);
}
