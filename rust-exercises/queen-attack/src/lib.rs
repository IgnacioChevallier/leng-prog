#[derive(Debug, PartialEq, Copy, Clone)]
pub struct ChessPosition(i32, i32);

#[derive(Debug)]
pub struct Queen {
    position: ChessPosition,
}

impl ChessPosition {
    pub fn new(rank: i32, file: i32) -> Option<Self> {
        if rank >= 0 && rank <= 7 && file >= 0 && file <= 7 {
            Some(ChessPosition(rank, file))
        } else {
            None
        }
    }
}

impl Queen {
    pub fn new(position: ChessPosition) -> Self {
        Queen { position }
    }

    pub fn can_attack(&self, other: &Queen) -> bool {
        self.same_row(other) || self.same_col(other) || self.same_diagonal(other)
    }

    pub fn same_row(&self, other: &Queen) -> bool {
        self.position.1 == other.position.1
    }

    pub fn same_col(&self, other: &Queen) -> bool {
        self.position.0 == other.position.0
    }

    pub fn same_diagonal(&self, other: &Queen) -> bool {
        let possible_diagonal_moves = self.get_possible_diagonal_moves();
        possible_diagonal_moves.contains(&other.position)
    }

    pub fn get_possible_diagonal_moves(&self) -> Vec<ChessPosition> {
        let mut result = Vec::new();
        let mut n = self.position;

        while n.0 < 8 && n.1 < 8 {
            n = ChessPosition(n.0 + 1, n.1 + 1);
            result.push(n);
        }

        n = self.position;

        while n.0 >= 0 && n.1 < 8 {
            n = ChessPosition(n.0 - 1, n.1 + 1);
            result.push(n);
        }

        n = self.position;

        while n.0 >= 0 && n.1 >= 0 {
            n = ChessPosition(n.0 - 1, n.1 - 1);
            result.push(n);
        }

        n = self.position;

        while n.0 < 8 && n.1 >= 0 {
            n = ChessPosition(n.0 + 1, n.1 - 1);
            result.push(n);
        }

        result
    }
}
