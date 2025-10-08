//! collection of my code in rust

#![no_std]

#[macro_use] extern crate alloc;

pub mod ege1 {
    /// the first code with tasks from EGE exam
    pub fn ex1() -> usize {
        let text = include_str!("11.txt");
        let newtext = text.replace("INFINITY", "@");
        let mut pos = alloc::vec::Vec::new();
        for (i, char) in newtext.chars().enumerate() {
            if char == '@' {
                pos.push(i);
            }
        }
        let mut mx = 0;
        for i in 0..(pos.len() - 1001) {
            mx = mx.max(pos[i + 1001] - pos[i] - 1 + 7000 + 14);
        }
        mx
    }

    #[cfg(test)]
    mod tests {
        use super::ex1;

        #[test]
        fn test_all() {
            let ex1 = ex1();
            assert_eq!(ex1, 36747);
        }
    }
}

pub mod first_word {
    use alloc::borrow::ToOwned;

    pub fn first_word(input: &str) -> alloc::string::String {
        input.find(' ').map_or_else(|| input.trim(), |n| input[..n].trim()).to_owned()
    }

    #[cfg(test)]
    mod tests {
        use super::first_word;

        #[test]
        fn test_all() {
            assert_eq!(first_word("hello world  "), "hello");
            assert_eq!(first_word("    no"), "");
            assert_eq!(first_word("coffee of tea"), "coffee");
        }
    }
}

pub mod integral {
    extern crate std;

    use {
        meval::{eval_str_with_context, Context},
        std::{
            boxed::Box,
            format,
            string::String,
            sync::{
                mpsc::{self, SendError},
                Arc,
            },
            thread,
        },
    };

    fn f(x: f64, expr: &str) -> f64 {
        let mut context = Context::new();
        context.var("x", x);
        eval_str_with_context(expr, &context).expect("wrong variable!")
    }

    fn lrect(a: f64, b: f64, n: f64, expr: &str) -> f64 {
        let mut s: f64 = 0.0;
        for i in 0..=(n - 1.0) as u64 {
            s += f(a + i as f64 * (b - a) / n, expr);
        }
        s * ((b - a) / n)
    }

    fn rrect(a: f64, b: f64, n: f64, expr: &str) -> f64 {
        let mut s: f64 = 0.0;
        for i in 1..=n as u64 {
            s += f(a + i as f64 * (b - a) / n, expr);
        }
        s * ((b - a) / n)
    }

    fn mrect(a: f64, b: f64, n: f64, expr: &str) -> f64 {
        let mut s: f64 = 0.0;
        for i in 0..=(n - 1.0) as u64 {
            s += f(a + (b - a) * (2.0 * i as f64 + 1.0) / (2.0 * n), expr);
        }
        s * ((b - a) / n)
    }

    fn trapezoid(a: f64, b: f64, n: f64, expr: &str) -> f64 {
        let mut s: f64 = 0.0;
        for i in 0..=(n - 1.0) as u64 {
            s += (f(a + i as f64 * (b - a) / n, expr) + f(a + (i + 1) as f64 * (b - a) / n, expr)) / 2.0;
        }
        s * ((b - a) / n)
    }

    /// oint algorithms
    pub fn integral(a: f64, b: f64, eps: f64, expr: &'static str) -> Result<String, Box<dyn std::error::Error>> {
        let (txl, rx) = mpsc::channel();
        let (txr, txm, txt) = (txl.clone(), txl.clone(), txl.clone());

        let expr = Arc::new(expr);
        let (exprr, exprm, exprt) = (Arc::clone(&expr), Arc::clone(&expr), Arc::clone(&expr));

        thread::spawn(move || -> Result<(), SendError<_>> {
            let (mut n, mut s1, mut s2) = (1.0, 0.0, f(a, &expr) * (b - a));
            while (s2 - s1).abs() > eps {
                n *= 2.0;
                (s1, s2) = (s2, lrect(a, b, n, &expr));
            }
            txl.send(format!("left: {s2}\n"))?;
            Ok(())
        });

        thread::spawn(move || -> Result<(), SendError<_>> {
            let (mut n, mut s1, mut s2) = (1.0, 0.0, f(a, &exprr) * (b - a));
            while (s2 - s1).abs() > eps {
                n *= 2.0;
                (s1, s2) = (s2, rrect(a, b, n, &exprr));
            }
            txr.send(format!("right: {s2}\n"))?;
            Ok(())
        });

        thread::spawn(move || -> Result<(), SendError<_>> {
            let (mut n, mut s1, mut s2) = (1.0, 0.0, f((a + b) / 2.0, &exprm) * (b - a));
            while (s2 - s1).abs() > eps {
                n *= 2.0;
                (s1, s2) = (s2, mrect(a, b, n, &exprm));
            }
            txm.send(format!("middle: {s2}\n"))?;
            Ok(())
        });

        thread::spawn(move || -> Result<(), SendError<_>> {
            let (mut n, mut s1, mut s2) = (1.0, 0.0, (b - a) * (f(a, &exprt) + f(b, &exprt)) / 2.0);
            while (s2 - s1).abs() > eps {
                n *= 2.0;
                (s1, s2) = (s2, trapezoid(a, b, n, &exprt));
            }
            txt.send(format!("trapezoid: {s2}\n"))?;
            Ok(())
        });

        let mut ret = String::new();
        for res in rx {
            ret.push_str(&res);
        }
        Ok(ret)
    }
}

pub mod last_word {
    use alloc::borrow::ToOwned;

    /// same as lastword.cpp, but with Rust syntax
    pub fn last_word(input: &str) -> alloc::string::String {
        input.trim().rfind(' ').map_or(input.trim().to_owned(), |n| input[n + 1..].trim().to_owned())
    }

    #[cfg(test)]
    mod tests {
        use super::last_word;

        #[test]
        fn test_all() {
            assert_eq!(last_word("hello world  "), "world");
            assert_eq!(last_word("    no"), "no");
            assert_eq!(last_word("coffee of tea"), "tea");
        }
    }
}

pub mod longman {
    use alloc::borrow::ToOwned;

    pub fn longman(input: &str) -> alloc::string::String {
        let mut maxstr = "";
        for word in input.split_whitespace() {
            maxstr = if word.len() > maxstr.len() { word } else { maxstr };
        }
        maxstr.to_owned()
    }
}

pub mod longman2 {
    extern crate std;
    use {
        rayon::prelude::*,
        std::{string::String, vec::Vec},
    };

    /// find a maximum HEX number in string
    pub fn longman2(data: &str) -> String {
        assert!(data.is_ascii(), "non-ASCII characters are not allowed");
        let nums: Vec<String> = data
            .chars()
            .fold((String::new(), vec![]), |(mut word, mut nums), char| {
                if char.is_ascii_digit() || ['a', 'b', 'c', 'd', 'e', 'f'].contains(&char.to_ascii_lowercase()) {
                    word.push(char);
                } else {
                    if !word.is_empty() {
                        nums.push(word.clone());
                    }
                    word.clear();
                }
                (word, nums)
            })
            .1;
        nums.into_par_iter().max_by_key(|num| usize::from_str_radix(num, 16).unwrap()).unwrap()
    }

    #[cfg(test)]
    mod tests {
        use super::longman2;

        #[test]
        fn test_all() {
            assert_eq!(longman2("af5Y3d"), "af5");
        }
    }
}

pub mod mask1 {
    extern crate std;
    use {rayon::prelude::*, regex::Regex, std::string::ToString};

    pub fn fn_match() -> Result<std::vec::Vec<usize>, std::boxed::Box<dyn std::error::Error>> {
        let mask = Regex::new("^123.*567.?$")?;
        Ok((169..1_000_000_000usize)
            .into_par_iter()
            .filter(|x| x % 169 == 0 && mask.is_match(&x.to_string()))
            .collect())
    }

    #[cfg(test)]
    mod tests {
        use super::fn_match;

        #[test]
        fn test_all() {
            let mask = fn_match().unwrap();
            assert_eq!(mask, [
                12325677, 12385672, 123157567, 123165679, 123225674, 123326567, 123495567, 123515678, 123575673,
                123664567, 123833567, 123865677, 123925672
            ])
        }
    }
}

pub mod mcko {
    //! Russian text ahead: данный код решает номера из диагностики МЦКО - для московских школьников.
    //! Хоть и кажется, что лучше использовать Python для всего этого, Rust достаточно высокоуровневый,
    //! чтобы у нас была возможность более-менее просто решать эти номера. Номера взяты из
    //! [комплекта](https://mcko.ru/uploads/documents/informatika_-10-klass_komplekt_1-91f3befffaba6eed.zip).
    //! Здесь приведены решения лишь 4 номеров, так как остальные либо вообще не решаются
    //! программированием, либо гораздо быстрее решаются руками.

    extern crate std;

    use {
        rayon::prelude::*,
        std::{error::Error, format},
    };

    /// Номер 2
    /// Автомат обрабатывает натуральное число N по следующему алгоритму:
    /// 1) Строится двоичная запись числа N.
    /// 2) К полученной записи дописываются разряды по следующему принципу: если число чётное, то справа дописывается
    ///    10, если нечётное – слева дописывается 1 и справа 00.
    /// 3) Результат переводится в десятичную систему и выводится на экран. В результате работы автомата на экране
    ///    появилось число, большее 107. Для какого наименьшего N данная ситуация возможна? В ответе найденное число N
    ///    запишите в десятичной системе.
    ///
    /// Ответ: 11
    pub fn n2(mut n: u16) -> u16 {
        loop {
            let m = u32::from_str_radix(&if n % 2 == 0 { format!("{n:b}10") } else { format!("1{n:b}00") }, 2).unwrap();
            if m > 107 {
                return n;
            }
            n += 1;
        }
    }

    /// Номер 10
    /// Редактор получает на вход строку цифр и преобразовывает её. Редактор может выполнять две
    /// команды, в обеих командах v и w обозначают цепочки цифр. Заменить (v, w) Эта команда заменяет
    /// в строке первое слева вхождение цепочки v на цепочку w. Нашлось (v) Эта команда
    /// проверяет, встречается ли цепочка v в строке исполнителя Редактор. Если она встречается, то
    /// команда возвращает логическое значение «истина», в противном случае возвращает значение «ложь».
    /// Строка при этом не изменяется. Дана программа для исполнителя Редактор:
    /// НАЧАЛО
    /// ПОКА нашлось (35) ИЛИ нашлось (355) ИЛИ нашлось (3444)
    ///  ЕСЛИ нашлось (35)
    ///  ТО заменить (35, 4)
    ///  ИНАЧЕ
    ///  ЕСЛИ нашлось (355)
    ///  ТО заменить (355, 4)
    ///  ИНАЧЕ заменить (3444, 3)
    ///  КОНЕЦ ЕСЛИ
    ///  КОНЕЦ ЕСЛИ
    /// КОНЕЦ ПОКА
    /// КОНЕЦ
    /// Какая строка получится в результате применения приведённой выше программы к строке вида 3…34…4
    /// (6 троек и 75 четвёрок)? В ответе запишите полученную строку.
    ///
    /// Ответ: 333333
    pub fn n10() -> std::string::String {
        let mut input = std::string::String::from(
            "333333444444444444444444444444444444444444444444444444444444444444444444444444444",
        );
        while ["35", "355", "3444"].iter().any(|&s| input.contains(s)) {
            if input.contains("35") {
                input = input.replace("35", "4");
            } else if input.contains("355") {
                input = input.replace("355", "4");
            } else {
                input = input.replace("3444", "3");
            }
        }
        input
    }

    /// Номер 11
    /// Операнды арифметического выражения записаны в системе счисления с основанием 12:
    /// 154x3_12 + 1x365_12. В записи чисел переменной x обозначена неизвестная цифра из алфавита
    /// двенадцатеричной системы счисления. Определите значение x, при котором значение данного
    /// арифметического выражения кратно 13. Для найденного значения x вычислите частное от деления
    /// значения арифметического выражения на 13 и укажите его в ответе в десятичной системе счисления.
    /// Основание системы счисления в ответе указывать не нужно.
    ///
    /// Ответ: 4340
    pub fn n11() -> u32 {
        (0..12)
            .find_map(|i| {
                let sum = u32::from_str_radix(&format!("154{i:X}3"), 12).unwrap()
                    + u32::from_str_radix(&format!("1{i:X}365"), 12).unwrap();
                if sum % 13 == 0 {
                    Some(sum / 13)
                } else {
                    None
                }
            })
            .unwrap()
    }

    /// Номер 12
    /// В файле 12.txt содержится последовательность целых чисел. Элементы последовательности могут
    /// принимать целые значения от –100 000 до 100 000 включительно. Пусть N – минимальное число в
    /// последовательности, НЕ кратное 15. Определите количество пар элементов последовательности, в
    /// которых оба числа кратны N. В ответе запишите количество найденных пар, затем максимальную из
    /// сумм элементов таких пар. В данной задаче под парой подразумевается два идущих подряд элемента
    /// последовательности.
    ///
    /// Ответ: (157, 176024)
    pub fn n12() -> Result<(i32, i32), std::boxed::Box<dyn Error>> {
        let data: std::vec::Vec<i32> =
            include_str!("12.txt").par_lines().map(|line| line.trim().parse().unwrap()).collect();

        let min = *data.par_iter().filter(|&&x| x % 15 != 0).min().unwrap();

        Ok(data
            .par_windows(2)
            .filter(|t| t[0] % min == 0 && t[1] % min == 0)
            .fold(|| (0, 0), |(count, max), t| (count + 1, max.max(t[0] + t[1])))
            .reduce(|| (0, 0), |(count1, max1), (count2, max2)| (count1 + count2, max1.max(max2))))
    }

    #[cfg(test)]
    mod tests {
        use super::*;

        #[test]
        fn test_all() {
            assert_eq!(n2(1), 11);
            assert_eq!(n10(), "333333");
            assert_eq!(n11(), 4340);
            assert_eq!(n12().unwrap(), (157, 176024));
        }
    }
}

pub mod pop {
    extern crate std;
    use std::{println, thread::sleep, time::Duration};

    /// Russian poem
    #[allow(unconditional_recursion)]
    pub fn pop() -> ! {
        println!("У попа была собака,");
        sleep(Duration::from_secs_f32(1.5));
        println!("Он её любил.");
        sleep(Duration::from_secs_f32(1.5));
        println!("Она съела кусок мяса,");
        sleep(Duration::from_secs_f32(1.5));
        println!("Он её убил.");
        sleep(Duration::from_secs_f32(1.5));
        println!("В землю закопал,");
        sleep(Duration::from_secs_f32(1.5));
        println!("И написал он, что");
        sleep(Duration::from_secs_f32(1.5));
        pop()
    }
}

pub mod probnik {
    use itertools::Itertools;

    pub fn probnik() -> usize {
        let mut k = 0;
        for word in "ПРОБНИК".chars().permutations(7) {
            let len = word.len();
            if !"ОИ".contains(word[0]) && !"ОИ".contains(word[len - 1]) {
                let mut f = true;
                for i in 0..len - 1 {
                    if ["ОИ", "ИО"]
                        .into_iter()
                        .any(|s| s.contains(&word[i..i + 2].iter().collect::<alloc::string::String>()))
                    {
                        f = false;
                        break;
                    }
                }
                if f {
                    k += 1;
                }
            }
        }
        k
    }

    #[cfg(test)]
    mod tests {
        use super::probnik;

        #[test]
        fn test_all() {
            assert_eq!(probnik(), 1440);
        }
    }
}

pub mod resheto {
    extern crate std;
    use std::{
        format,
        string::{String, ToString},
    };

    pub fn resheto(mut n: u64) -> String {
        let mut ret = String::new();
        let mut printed = false;
        let (mut count, m) = (0, n);
        while n % 2 == 0 {
            n /= 2;
            count += 1;
        }
        match count {
            1 => {
                ret.push('2');
                printed = true;
            },
            2.. => {
                ret.push_str(&format!("2^{count}"));
                printed = true;
            },
            _ => (),
        }
        for i in (3..).step_by(2).take_while(|i| i * i <= m) {
            count = 0;
            while n % i == 0 {
                n /= i;
                count += 1;
            }
            match count {
                1 =>
                    if printed {
                        ret.push_str(&format!("*{i}"));
                    } else {
                        ret.push_str(&i.to_string());
                        printed = true;
                    },
                2.. =>
                    if printed {
                        ret.push_str(&format!("*{i}^{count}"));
                    } else {
                        ret.push_str(&format!("{i}^{count}"));
                        printed = true;
                    },
                _ => (),
            }
        }
        if n > 2 {
            if printed {
                ret.push_str(&format!("*{n}"));
            } else {
                ret.push_str(&n.to_string());
            }
        }
        ret
    }
}

pub mod sfml_example {
    extern crate std;
    use {
        sfml::{
            graphics::{CircleShape, Color, FloatRect, RenderTarget, RenderWindow, Shape, Transformable, View},
            system::Vector2f,
            window::{Event, Key, Style},
        },
        std::boxed::Box,
    };

    pub fn window() -> Result<(), Box<dyn std::error::Error>> {
        let mut window = RenderWindow::new(
            (200, 200),
            "blazingly fast, memory-safe sfml window",
            Style::RESIZE | Style::CLOSE,
            &Default::default(),
        )?;
        let mut shape = CircleShape::new(100., 30);
        shape.set_fill_color(Color::GREEN);

        while window.is_open() {
            while let Some(event) = window.poll_event() {
                match event {
                    Event::Closed | Event::KeyPressed { code: Key::Escape, .. } => window.close(),
                    Event::Resized { width, height } => {
                        let view = View::from_rect(FloatRect::new(0., 0., width as f32, height as f32))?;
                        let radius = width.min(height) as f32 / 2.;
                        shape.set_radius(radius);
                        shape.set_origin(Vector2f::new(radius, radius));
                        shape.set_scale(Vector2f::new(width as f32 / height as f32, 1.));
                        shape.set_position(Vector2f::new(width as f32 / 2., height as f32 / 2.));
                        window.set_view(&view);
                    },
                    _ => (),
                }
            }
            window.clear(Color::rgb(0, 0, 0));
            window.draw(&shape);
            window.display();
        }

        Ok(())
    }

    // it laucnhes window, which is obviously hard to test in CI
    // #[cfg(test)]
    // mod tests {
    //     extern crate std;
    //     use std::boxed::Box;
    //     use super::window;
    //     #[test]
    //     fn launch_window() -> Result<(), Box<dyn std::error::Error>> {
    //         window()
    //     }
    // }
}

pub mod speedometer {
    //! a programm to test speed of each language (Rust implementation)

    #[inline]
    #[allow(dead_code)]
    pub const fn s() -> u32 {
        let mut n = 1;
        while n < 1_000_000_000 {
            n += 1;
        }
        n
    }

    #[cfg(test)]
    mod tests {
        extern crate std;
        use {super::s, std::print};

        #[test]
        fn print() { print!("{}", s()) }
    }
}

pub mod tumba_umba {
    use alloc::{
        string::{String, ToString},
        vec::Vec,
    };

    /// same as tumba-umba.cpp, but for only the first task
    pub fn tumba_umba(alpha: &[char], k: usize) -> String {
        let mut ret = String::new();
        let mut words: Vec<String> = Vec::new();
        let count = gen(alpha, &mut vec![' '; k], 0, &mut words);
        words.sort();
        for word in words {
            ret.push_str(&word);
        }
        ret.push_str(&count.to_string());
        ret
    }

    fn gen(alpha: &[char], current: &mut Vec<char>, i: usize, words: &mut Vec<String>) -> usize {
        if i == current.len() {
            let mut temp = current.iter().collect::<String>();
            temp.push('\n');
            words.push(temp);
            return 1;
        }

        let mut count = 0;
        for j in 0..alpha.len() {
            current[i] = alpha[j];
            count += gen(alpha, current, i + 1, words);
        }
        count
    }
}
