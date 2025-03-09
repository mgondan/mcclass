:- module(table, [ htmltable//4, htmlform//3, download//1, navtabs//3, tabcontents//2 ]).

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

tbody(RowNames, Data)
--> { pairs_keys_values(Pairs, RowNames, Data) },
    html(tbody(\foreach(member(R-D, Pairs), html(\trow(R, D))))).

% produces a line at the bottom
tfoot -->
    html(tfoot(tr(" "))).

trow(Row, Data) -->
    html(tr([th(scope(row), Row) | \foreach(member(D, Data), html(td(D)))])).

htmlform(Question, Action, Response) -->
    html(div(class(card), div(class('card-body'),
      [ h4(class('card-title'), [a(id(question), []), "Question"]),
        p(class('card-text'), Question),
        form([class(form), method(post), action("#~w"-[Action])],
          [ div(class('input-group mb-3'),
              [ div(class('input-group-prepend'),
                  span(class('input-group-text'), "Response")),
                input([class('form-control'), type(text), name(resp), 
                  value(Response)]),
                input([type(hidden), name(task), value(Action)]),
                div(class('input-group-append'),
                  button([class('btn btn-primary'), type(submit)], "Submit"))
              ])
          ])
      ]))).

download(Task) -->
    html(form(method(post),
      button([class('btn btn-secondary'), name(download), value(Task)], "Download data"))).

navtabs(Topic, Tasks, Current)
--> html(nav(div([class('nav nav-tabs'), id('nav-tab'), role(tablist)],
      \foreach(member(T, Tasks), html(\navtab(Topic, T, Current)))))).

navtab(Topic, Task, Task)
--> {Topic:task_label(Topic, Task, Label)},
    html(button([class('nav-link active'), id('nav-~w-tab'-[Task]),
      'data-bs-toggle'(tab), 'data-bs-target'('#nav-~w'-[Task]),
      type(button), role(tab), 'aria-controls'('nav-~w'-[Task]),
      'aria-selected'(true)], Label)).

navtab(Topic, Task, _)
--> {Topic:task_label(Topic, Task, Label)},
    html(button([class('nav-link'), id('nav-~w-tab'-[Task]), 
      'data-bs-toggle'(tab), 'data-bs-target'('#nav-~w'-[Task]), 
      type(button), role(tab), 'aria-controls'('nav-~w'-[Task]), 
      'aria-selected'(false)], 
      Label)).

tabcontents(Contents, Current)
--> html(div([class('tab-content'), id('nav-tabContent')],
      \foreach(member(T-C, Contents), html(\tabcontent(C, T, Current))))).

tabcontent(Content, Task, Task)
--> html(div([class('tab-pane fade show active'), id('nav-~w'-[Task]), 
        role(tabpanel), 'aria-labelledby'('nav-~w-tab'-[Task]), tabindex(0)],
      Content)).

tabcontent(Content, Task, _)
--> html(div([class('tab-pane fade'), id('nav-~w'-[Task]),
        role(tabpanel), 'aria-labelledby'('nav-~w-tab'-[Task]), tabindex(0)],
      Content)).

test :-
    phrase(html(\htmltable("A table", 
        [r1, r2, r3], 
        [c1, c2, c3], 
        [[11, 12, 13], [21, 22, 23], [31, 32, 33]])), Table),
    writeln(Table).
