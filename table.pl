:- module(table, [htmltable//4]).

:- use_module(library(http/html_write)).
:- use_module(library(dcg/high_order)).

htmltable(Caption, RowNames, ColNames, Data) -->
    html(table(class("table caption-top"), 
        [\caption(Caption), \thead(ColNames), \tbody(RowNames, Data)])).

caption(Caption) -->
    html(caption(Caption)).

thead(ColNames) -->
    html(thead(tr(\foreach(member(C, ColNames), html(th(scope(col), C)))))).

tbody(RowNames, Data) -->
    {pairs_keys_values(Pairs, RowNames, Data)},
    html(tbody(\foreach(member(R-D, Pairs), html(\trow(R, D))))).

trow(Row, Data) -->
    html(tr([th(scope(row), Row) | \foreach(member(D, Data), html(td(D)))])).

test -->
    html(\htmltable("A table", [r1, r2, r3], [c1, c2, c3], 
      [[11, 12, 13], [21, 22, 23], [31, 32, 33]])).
