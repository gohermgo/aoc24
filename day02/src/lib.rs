use std::ops::Sub;

pub trait Row {
    type Elt;
    fn is_safe(&self) -> bool;

    fn get(&self, index: usize) -> Option<&Self::Elt>;
    fn all_increasing(&self) -> bool
    where
        Self::Elt: PartialOrd,
    {
        let Some(mut prev) = self.get(0) else {
            return false;
        };
        let mut index = 1;
        while let Some(current) = self.get(index) {
            if current <= prev {
                return false;
            };
            index += 1;
            prev = current;
        }
        true
    }
    fn all_decreasing(&self) -> bool
    where
        Self::Elt: PartialOrd,
    {
        let Some(mut prev) = self.get(0) else {
            return false;
        };
        let mut index = 1;
        while let Some(current) = self.get(index) {
            if current >= prev {
                return false;
            };
            index += 1;
            prev = current;
        }
        true
    }
    fn delta_list(&self) -> Option<Vec<<&Self::Elt as Sub>::Output>>
    where
        Self::Elt: PartialOrd,
        for<'a> &'a Self::Elt: Sub,
    {
        let mut prev = self.get(0)?;
        let mut index = 1;
        let mut delta_buf = vec![];
        while let Some(current) = self.get(index) {
            delta_buf.push(if current > prev {
                current - prev
            } else {
                prev - current
            });
            index += 1;
            prev = current;
        }
        Some(delta_buf)
    }
}

pub trait IntoRows {
    type RowElt;
    type IntoRow: Row<Elt = Self::RowElt> + Ord;
    fn into_rows(self) -> Option<Rows<Self::IntoRow>>;
    fn into_sorted_rows(self) -> Option<Rows<Self::IntoRow>>
    where
        Self: Sized,
        Self::IntoRow: Ord,
    {
        fn sort_rows<T: IntoRows>(mut a: Rows<T::IntoRow>) -> Rows<T::IntoRow>
        where
            T::IntoRow: Ord,
        {
            a.value.sort();
            a
        }
        self.into_rows().map(|rows| sort_rows::<Self>(rows))
    }
    // fn into_distances(self) -> Option<impl Iterator<Item = u32>>
    // where
    //     Self: Sized,
    // {
    //     fn fold_lists_into_distances((a_nums, b_nums): Lists) -> impl Iterator<Item = u32> {
    //         assert!(a_nums.is_sorted());
    //         assert!(b_nums.is_sorted());
    //         a_nums.into_iter().zip(b_nums).map(|(a, b)| a.abs_diff(b))
    //     }
    //     self.into_sorted_rows().map(fold_lists_into_distances)
    // }
    // fn into_occurences(self) -> Option<impl IntoIterator<Item = OccurencePair>>
    // where
    //     Self: Sized,
    // {
    //     fn fold_lists_into_similarity_scores((a_nums, b_nums): Lists) -> Vec<OccurencePair> {
    //         let mut scores = Vec::new();
    //         for k in a_nums {
    //             let v =
    //                 b_nums.iter().fold(
    //                     Default::default(),
    //                     |acc: u32, elt| if k.eq(elt) { acc + 1 } else { acc },
    //                 );
    //             scores.push((k, v));
    //         }
    //         scores
    //     }
    //     self.into_rows().map(fold_lists_into_similarity_scores)
    // }
}
impl Row for Vec<u32> {
    type Elt = u32;
    fn get(&self, index: usize) -> Option<&Self::Elt> {
        self.as_slice().get(index)
    }

    fn is_safe(&self) -> bool {
        if !(self.all_increasing() || self.all_decreasing()) {
            return false;
        }
        let Some(deltas) = self.delta_list() else {
            return false;
        };
        deltas.into_iter().all(|value| (1..=3).contains(&value))
    }
}
pub struct Rows<R> {
    value: Vec<R>,
}
impl<R> IntoIterator for Rows<R>
where
    R: IntoIterator,
{
    type Item = R;
    type IntoIter = std::vec::IntoIter<R>;
    fn into_iter(self) -> Self::IntoIter {
        self.value.into_iter()
    }
}
impl<R> Rows<R>
where
    R: Row,
{
    fn safe_report_count(&self) -> usize {
        let mut count = 0;
        for row in self.value.iter() {
            if row.is_safe() {
                count += 1;
            }
        }
        count
    }
}
impl IntoRows for std::str::Lines<'_> {
    type IntoRow = Vec<u32>;
    type RowElt = u32;
    fn into_rows(self) -> Option<Rows<Self::IntoRow>> {
        let mut buf = vec![];
        for line in self {
            let split = line.split_whitespace();
            let mut seg_buf = vec![];
            for seg in split {
                let digit: u32 = seg.parse().ok()?;
                seg_buf.push(digit);
            }
            buf.push(seg_buf);
        }
        Some(Rows { value: buf })
    }
}
impl IntoRows for std::fs::File {
    type IntoRow = Vec<u32>;
    type RowElt = u32;
    fn into_rows(mut self) -> Option<Rows<Self::IntoRow>> {
        let mut s = String::new();
        let _ = std::io::Read::read_to_string(&mut self, &mut s).ok()?;
        s.lines().into_rows()
    }
}
#[test]
fn into_rows_works() {
    let lines = crate::tests::TEST_INPUT.lines();
    let res = lines.into_rows();
    assert!(res.is_some())
}
pub mod part01 {
    //! Part 1 solution
}
pub mod part02 {
    //! Part 2 solution

    use crate::IntoRows;
    pub fn calculate_safe_report_count(input: impl IntoRows) -> usize {
        let Some(rows) = input.into_rows() else {
            return 0;
        };
        rows.safe_report_count()
    }
}
#[cfg(test)]
mod tests {
    pub const TEST_INPUT: &str = r"7 6 4 2 1
1 2 7 8 9
9 7 6 2 1
1 3 2 4 5
8 6 4 4 1
1 3 6 7 9";
    use super::*;

    mod part01 {
        use super::*;
        #[test]
        fn each_safety_correct() {
            let lines = TEST_INPUT.lines();
            let rows = lines.into_rows().unwrap();
            let mut rows_iter = rows.into_iter();
            // Row 1 safe
            assert!(rows_iter.next().unwrap().is_safe());
            // Row 2 unsafe
            assert!(!rows_iter.next().unwrap().is_safe());
            // Row 3 unsafe
            assert!(!rows_iter.next().unwrap().is_safe());
            // Row 4 unsafe
            assert!(!rows_iter.next().unwrap().is_safe());
            // Row 5 unsafe
            assert!(!rows_iter.next().unwrap().is_safe());
            // Row 6 safe
            assert!(rows_iter.next().unwrap().is_safe());

            assert!(rows_iter.next().is_none())
        }
        #[test]
        fn final_count_correct() {
            let lines = TEST_INPUT.lines();
            let rows = lines.into_rows().unwrap();
            let count = rows.safe_report_count();
            assert_eq!(count, 2)
        }
    }
    mod part02 {
        use super::*;
    }
}
