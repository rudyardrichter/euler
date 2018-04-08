use std::io::prelude::Read;
use std::fs::File;

fn load_data() -> Vec<Vec<usize>> {
    let mut file = File::open("data/011/grid.txt").expect("missing file");
    let mut contents = String::new();
    file.read_to_string(&mut contents)
        .expect("couldn't read file");
    contents
        .lines()
        .map(|line| {
            line.split_whitespace()
                .map(|word| word.parse::<usize>().unwrap())
                .collect::<Vec<usize>>()
        })
        .collect()
}

#[derive(PartialEq)]
enum Direction {
    Horizontal,
    Vertical,
    DiagonalLeft,
    DiagonalRight,
}

fn windows_max_by<T>(
    matrix: &[&[T]],
    direction: Direction,
    window_size: usize,
    window_eval: &Fn(&[T]) -> T,
) -> T
where
    T: Copy + Ord,
{
    let matrix_iterate = |row_bounds: (usize, usize),
                          col_bounds: (usize, usize),
                          offset_coord: &Fn(usize, usize, usize) -> (usize, usize)|
     -> T {
        let mut result: Option<T> = None;
        for i_row in row_bounds.0..row_bounds.1 {
            for i_col in col_bounds.0..col_bounds.1 {
                let mut slice = vec![];
                for offset in 0..window_size {
                    let (x, y) = offset_coord(i_row, i_col, offset);
                    slice.push(matrix[x][y]);
                }
                let window_result = Some(window_eval(slice.as_slice()));
                if window_result > result {
                    result = window_result;
                }
            }
        }
        result.unwrap()
    };

    match direction {
        Direction::Horizontal => matrix
            .iter()
            .map(|row| row.windows(window_size).map(window_eval).max().unwrap())
            .max()
            .unwrap(),
        Direction::Vertical => {
            let row_bounds = (0, matrix.len() - window_size + 1);
            let col_bounds = (0, matrix[0].len());
            let offset_coord = |x, y, offset| (x + offset, y);
            matrix_iterate(row_bounds, col_bounds, &offset_coord)
        }
        Direction::DiagonalLeft => {
            let row_bounds = (0, matrix.len() - window_size + 1);
            let col_bounds = (window_size, matrix[0].len());
            let offset_coord = |x, y, offset| (x + offset, y - offset);
            matrix_iterate(row_bounds, col_bounds, &offset_coord)
        }
        Direction::DiagonalRight => {
            let row_bounds = (0, matrix.len() - window_size + 1);
            let col_bounds = (0, matrix[0].len() - window_size + 1);
            let offset_coord = |x, y, offset| (x + offset, y + offset);
            matrix_iterate(row_bounds, col_bounds, &offset_coord)
        }
    }
}

fn main() -> () {
    let data = load_data();
    let slices = data.iter().map(|line| line.as_slice()).collect::<Vec<&[usize]>>();
    let matrix = slices.as_slice();
    let mut results = vec![];
    let directions = vec![Direction::Horizontal, Direction::Vertical, Direction::DiagonalLeft, Direction::DiagonalRight];
    let window_eval = |window: &[usize]| window.iter().product();
    for direction in directions {
        results.push(windows_max_by(matrix, direction, 4, &window_eval));
    }
    let answer = results.iter().max().unwrap();
    print!("{}\n", answer);
}
