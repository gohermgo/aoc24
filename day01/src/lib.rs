mod into_lists;
pub use into_lists::IntoLists;
use into_lists::{Lists, OccurencePair};

pub mod part_1 {
    //! Part 1 solution
    use super::*;
    pub fn calculate_final_distance(src: impl IntoLists) -> Option<u32> {
        /// Folds a zipped pair of lists into distances
        fn fold_lists_into_distances((a_nums, b_nums): Lists) -> impl Iterator<Item = u32> {
            assert!(a_nums.is_sorted());
            assert!(b_nums.is_sorted());
            a_nums.into_iter().zip(b_nums).map(|(a, b)| a.abs_diff(b))
        }
        src.into_sorted_lists()
            .map(fold_lists_into_distances)
            .map(Iterator::sum)
    }
}
pub mod part_2 {
    //! Part 2 solution
    use super::*;
    pub fn calculate_final_similarity_score(src: impl IntoLists) -> Option<u32> {
        /// Folds a list of zipped pairs into similarity-scores
        fn fold_lists_into_similarities(
            scores: impl IntoIterator<Item = OccurencePair>,
        ) -> impl Iterator<Item = u32> {
            scores.into_iter().map(|(a, b)| a * b)
        }
        src.into_occurences()
            .map(fold_lists_into_similarities)
            .map(Iterator::sum)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    pub const TEST_INPUT: &str = r"3   4
4   3
2   5
1   3
3   9
3   3";
    mod part_1 {
        use super::*;
        #[test]
        fn each_distance_correct() {
            let lines = TEST_INPUT.lines();
            let distances = lines.into_distances().unwrap();
            let mut distances = distances.into_iter();
            assert_eq!(distances.next(), Some(2));
            assert_eq!(distances.next(), Some(1));
            assert_eq!(distances.next(), Some(0));
            assert_eq!(distances.next(), Some(1));
            assert_eq!(distances.next(), Some(2));
            assert_eq!(distances.next(), Some(5));
            assert_eq!(distances.next(), None)
        }
        #[test]
        fn final_distance_is_correct() {
            let lines = TEST_INPUT.lines();
            let final_distance = crate::part_1::calculate_final_distance(lines).unwrap();
            assert_eq!(final_distance, 11);
        }
    }
    mod part_2 {
        use super::*;
        #[test]
        fn each_similarity_score_correct() {
            let lines = TEST_INPUT.lines();
            let scores = lines.into_occurences().unwrap();
            let mut scores = scores.into_iter();
            assert_eq!(scores.next(), Some((3, 3)));
            assert_eq!(scores.next(), Some((4, 1)));
            assert_eq!(scores.next(), Some((2, 0)));
            assert_eq!(scores.next(), Some((1, 0)));
            assert_eq!(scores.next(), Some((3, 3)));
            assert_eq!(scores.next(), Some((3, 3)));
            assert_eq!(scores.next(), None);
        }
        #[test]
        fn final_similarity_score_is_correct() {
            let lines = TEST_INPUT.lines();
            let final_score = crate::part_2::calculate_final_similarity_score(lines).unwrap();
            assert_eq!(final_score, 31);
        }
    }
}
