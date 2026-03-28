# chrongler 0.2.0 _2026-03-28_
There are no real news, as this is the first release. 

Using *chrongler* (and a concordance of periods and their grouping and absolute 
dating made with `make_chrongler_conc()`) you can now:

-   group and un-group the categorical dating of objects (`group_periods()`, `ungroup_periods()`)
-   duplicate rows of objects according to their periods or groups (`duplicate_by()`)
-   add absolute dating based on the periods an object is dated to (`derive_dating()`)
-   add the period an object would be dated to based on absolute dating values (`derive_period()`)

And use the result to make your life easier (I hope). 
