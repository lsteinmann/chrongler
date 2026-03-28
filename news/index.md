# Changelog

## chrongler 0.2.0 *2026-03-28*

There are no real news, as this is the first release.

Using *chrongler* (and a concordance of periods and their grouping and
absolute dating made with
[`make_chrongler_conc()`](https://lsteinmann.github.io/chrongler/reference/make_chrongler_conc.md))
you can now:

- group and un-group the categorical dating of objects
  ([`group_periods()`](https://lsteinmann.github.io/chrongler/reference/group_periods.md),
  [`ungroup_periods()`](https://lsteinmann.github.io/chrongler/reference/ungroup_periods.md))
- duplicate rows of objects according to their periods or groups
  ([`duplicate_by()`](https://lsteinmann.github.io/chrongler/reference/duplicate_by.md))
- add absolute dating based on the periods an object is dated to
  ([`derive_dating()`](https://lsteinmann.github.io/chrongler/reference/derive_dating.md))
- add the period an object would be dated to based on absolute dating
  values
  ([`derive_period()`](https://lsteinmann.github.io/chrongler/reference/derive_period.md))

And use the result to make your life easier (I hope).
