:- module(table, [ htmltable//4, htmlform//3 ]).

/** <module> APA tables in HTML

Renders a HTML table with a caption at the top and borders above and below the
header and at the bottom.
*/

:- use_module(library(http/html_write)).
:- use_module(library(dcg/high_order)).
:- use_module(mathml).

%! htmltable(+Caption:string, +RowNames:list, +ColNames:list, +Data:list) is det
%
% @arg Caption The caption of the table (a string, or HTML tags)
% @arg RowNames A list with row names
% @arg ColNames A list with column names
% @arg Data A list of lists with the cells of the table
%
htmltable(Caption, RowNames, ColNames, Data) -->
    html(table(class("table caption-top"), 
      [ \caption(Caption), 
        \thead(ColNames), 
        \tbody(RowNames, Data),
        \tfoot
      ])).

caption(Caption) -->
    html(caption(Caption)).

thead(ColNames) -->
    html(thead(tr(\foreach(member(C, ColNames), html(th(scope(col), C)))))).

tbody(RowNames, Data) -->
    {pairs_keys_values(Pairs, RowNames, Data)},
    html(tbody(\foreach(member(R-D, Pairs), html(\trow(R, D))))).

% produces a line at the bottom
tfoot -->
    html(tfoot(tr(""))).

trow(Row, Data) -->
    html(tr([th(scope(row), Row) | \foreach(member(D, Data), html(td(D)))])).

htmlform(Question, Action, Response) -->
    html(div(class(card), div(class("card-body"),
      [ h4(class("card-title"), [a(id(question), []), "Question"]),
        p(class("card-text"), Question),
        form([class(form), method(post), action(Action)],
          [ div(class("input-group mb-3"),
              [ div(class("input-group-prepend"),
                  span(class("input-group-text"), "Response")),
                input([class("form-control"), type(text), name(resp), 
                  value(Response)]),
                div(class("input-group-append"),
                  button([class("btn btn-primary"), type(submit)], "Submit"))
              ])
          ])
      ]))).

test :-
    phrase(html(\htmltable("A table", 
        [r1, r2, r3], 
        [c1, c2, c3], 
        [[11, 12, 13], [21, 22, 23], [31, 32, 33]])), Table),
    writeln(Table).
