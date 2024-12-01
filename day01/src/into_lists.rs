use core::str;
use std::{fs, io};
pub(crate) type Lists = (Vec<u32>, Vec<u32>);
pub(crate) type OccurencePair = (u32, u32);
pub trait IntoLists {
    fn into_lists(self) -> Option<Lists>;
    fn into_sorted_lists(self) -> Option<Lists>
    where
        Self: Sized,
    {
        fn sort_lists((mut a, mut b): Lists) -> Lists {
            a.sort();
            b.sort();
            (a, b)
        }
        self.into_lists().map(sort_lists)
    }
    fn into_distances(self) -> Option<impl Iterator<Item = u32>>
    where
        Self: Sized,
    {
        fn fold_lists_into_distances((a_nums, b_nums): Lists) -> impl Iterator<Item = u32> {
            assert!(a_nums.is_sorted());
            assert!(b_nums.is_sorted());
            a_nums.into_iter().zip(b_nums).map(|(a, b)| a.abs_diff(b))
        }
        self.into_sorted_lists().map(fold_lists_into_distances)
    }
    fn into_occurences(self) -> Option<impl IntoIterator<Item = OccurencePair>>
    where
        Self: Sized,
    {
        fn fold_lists_into_similarity_scores((a_nums, b_nums): Lists) -> Vec<OccurencePair> {
            let mut scores = Vec::new();
            for k in a_nums {
                let v =
                    b_nums.iter().fold(
                        Default::default(),
                        |acc: u32, elt| if k.eq(elt) { acc + 1 } else { acc },
                    );
                scores.push((k, v));
            }
            scores
        }
        self.into_lists().map(fold_lists_into_similarity_scores)
    }
}

mod fold {
    use super::*;
    fn parse_str(s: impl AsRef<str>) -> Option<(u32, u32)> {
        const DELIMITER: &str = "   ";
        let (a, b) = s.as_ref().split_once(DELIMITER)?;
        a.parse().ok().zip(b.parse().ok())
    }

    pub fn str_line((mut acc_a, mut acc_b): Lists, elt: impl AsRef<str>) -> Option<Lists> {
        let (a, b) = parse_str(elt)?;
        acc_a.push(a);
        acc_b.push(b);
        Some((acc_a, acc_b))
    }

    pub fn io_line(lists: Lists, elt: io::Result<String>) -> Option<Lists> {
        let elt = elt.ok()?;
        str_line(lists, elt)
    }
}

impl IntoLists for str::Lines<'_> {
    fn into_lists(mut self) -> Option<Lists> {
        self.try_fold(Default::default(), fold::str_line)
    }
}
impl IntoLists for io::Lines<&[u8]> {
    fn into_lists(mut self) -> Option<Lists> {
        self.try_fold(Default::default(), fold::io_line)
    }
}

impl IntoLists for &[u8] {
    fn into_lists(self) -> Option<Lists> {
        io::BufRead::lines(self).into_lists()
    }
}
impl IntoLists for fs::File {
    fn into_lists(mut self) -> Option<Lists> {
        let mut content = Vec::new();
        let _ = io::Read::read_to_end(&mut self, &mut content).ok()?;
        io::BufRead::lines(content.as_slice()).into_lists()
    }
}

#[test]
fn works() {
    let lines = crate::tests::TEST_INPUT.lines();
    let res = lines.into_lists();
    assert!(res.is_some())
}
