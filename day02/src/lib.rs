use std::ops::Sub;
fn delta<'a, R: ?Sized>(
    row: &'a R,
    get_first: impl Fn() -> Option<&'a R::Elt>,
    init_counter: impl Fn() -> usize,
    mut pre_iteration_clause: impl FnMut(&mut usize),
) -> Option<Vec<<&'a R::Elt as Sub>::Output>>
where
    R: Row,
    R::Elt: PartialOrd,
    &'a R::Elt: Sub,
{
    let mut prev = get_first()?;
    let mut index = init_counter();
    let mut delta_buf = vec![];
    while let Some(current) = {
        pre_iteration_clause(&mut index);
        let elt = row.get(index);
        index += 1;
        elt
    } {
        delta_buf.push(if current > prev {
            current - prev
        } else {
            prev - current
        });
        prev = current;
    }
    Some(delta_buf)
}
fn check<'a, R: ?Sized>(
    row: &'a R,
    get_first: impl Fn() -> Option<&'a R::Elt>,
    init_counter: impl Fn() -> usize,
    mut pre_iteration_clause: impl FnMut(&mut usize),
    break_condition: impl Fn(&'a R::Elt, &'a R::Elt) -> bool,
) -> bool
where
    R: Row,
{
    let Some(mut prev) = get_first() else {
        return false;
    };
    let mut index = init_counter();
    while let Some(current) = {
        pre_iteration_clause(&mut index);
        let elt = row.get(index);
        index += 1;
        elt
    } {
        if break_condition(current, prev) {
            return false;
        }
        prev = current;
    }
    true
}
pub trait Row {
    type Elt;
    fn is_safe(&self) -> bool;
    fn is_safe_omitting(&self, index_of_omitted: usize) -> bool;
    fn is_safe_adjusted(&self) -> bool {
        for index in 0..self.len() {
            if self.is_safe_omitting(index) {
                return true;
            }
        }
        return false;
    }
    fn len(&self) -> usize;
    fn get(&self, index: usize) -> Option<&Self::Elt>;
    fn all_increasing(&self) -> bool
    where
        Self::Elt: PartialOrd,
    {
        check(
            self,
            || self.get(0),
            || 1,
            |_| {},
            |current, prev| current <= prev,
        )
    }
    fn all_increasing_omitting(&self, index_of_omitted: usize) -> bool
    where
        Self::Elt: PartialOrd,
    {
        check(
            self,
            || self.get(if index_of_omitted == 0 { 1 } else { 0 }),
            || if index_of_omitted == 0 { 2 } else { 1 },
            |index| {
                if index == &index_of_omitted {
                    *index = *index + 1;
                }
            },
            |current, prev| current <= prev,
        )
    }
    fn all_decreasing(&self) -> bool
    where
        Self::Elt: PartialOrd,
    {
        check(
            self,
            || self.get(0),
            || 1,
            |_| {},
            |current, prev| current >= prev,
        )
    }
    fn all_decreasing_omitting(&self, index_of_omitted: usize) -> bool
    where
        Self::Elt: PartialOrd,
    {
        check(
            self,
            || self.get(if index_of_omitted == 0 { 1 } else { 0 }),
            || if index_of_omitted == 0 { 2 } else { 1 },
            |index| {
                if index == &index_of_omitted {
                    *index = *index + 1;
                }
            },
            |current, prev| current >= prev,
        )
    }
    fn delta_list(&self) -> Option<Vec<<&Self::Elt as Sub>::Output>>
    where
        Self::Elt: PartialOrd,
        for<'a> &'a Self::Elt: Sub,
    {
        delta(self, || self.get(0), || 1, |_| {})
    }
    fn delta_list_omitting(
        &self,
        index_of_omitted: usize,
    ) -> Option<Vec<<&Self::Elt as Sub>::Output>>
    where
        Self::Elt: PartialOrd,
        for<'a> &'a Self::Elt: Sub,
    {
        delta(
            self,
            || self.get(if index_of_omitted == 0 { 1 } else { 0 }),
            || if index_of_omitted == 0 { 2 } else { 1 },
            |index| {
                if index == &index_of_omitted {
                    *index = *index + 1;
                }
            },
        )
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
}
impl Row for Vec<u32> {
    type Elt = u32;
    fn len(&self) -> usize {
        self.as_slice().len()
    }
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
    fn is_safe_omitting(&self, index_of_omitted: usize) -> bool {
        if !(self.all_increasing_omitting(index_of_omitted)
            || self.all_decreasing_omitting(index_of_omitted))
        {
            return false;
        }
        let Some(deltas) = self.delta_list_omitting(index_of_omitted) else {
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
    fn safe_adjusted_report_count(&self) -> usize {
        let mut count = 0;
        for row in self.value.iter() {
            if row.is_safe_adjusted() {
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

    use crate::IntoRows;
    pub fn calculate_safe_report_count(input: impl IntoRows) -> usize {
        let Some(rows) = input.into_rows() else {
            return 0;
        };
        rows.safe_report_count()
    }
}
pub mod part02 {
    //! Part 2 solution
    use crate::IntoRows;
    pub fn calculate_safe_adjusted_report_count(input: impl IntoRows) -> usize {
        let Some(rows) = input.into_rows() else {
            return 0;
        };
        rows.safe_adjusted_report_count()
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
            // Row 4 Safe
            assert!(rows_iter.next().unwrap().is_safe_omitting(1));
            // Row 5 Safe
            assert!(rows_iter.next().unwrap().is_safe_adjusted());
            // Row 6 Safe
            assert!(rows_iter.next().unwrap().is_safe());

            assert!(rows_iter.next().is_none())
        }

        #[test]
        fn final_count_correct() {
            let lines = TEST_INPUT.lines();
            let rows = lines.into_rows().unwrap();
            let count = rows.safe_adjusted_report_count();
            assert_eq!(count, 4)
        }
    }
}
