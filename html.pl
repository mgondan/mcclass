% Some HTML helper functions
:- use_module(library(dcg/high_order)).

nowrap(L) -->
    html(span(class('text-nowrap'), L)).

table(Head, Body) -->
    html(div(class("container shadow-sm my-3 mx-auto p-3 w-75"),
        table(class("table table-borderless m-auto"), 
	  [ \thead(Head), 
            \tbody(Body) 
          ]))).

thead([H | Row]) -->
    html(thead(style('border-top: 2px solid'),
        tr([th(H), \foreach(member(X, Row), html(th(class('text-center'), X)))]))).

tbody(Body) -->
    html(tbody(style('border-top: 1px solid; border-bottom: 2px solid'),
        \foreach(member(X, Body), html(\trow(X))))).

trow([H | Row]) -->
    html(tr([th(H), \foreach(member(X, Row), html(td(class('text-center'), X)))])).

download(Data) -->
    html(form(method('POST'),
        button(
          [ class('btn btn-secondary'), 
            name(download), value(Data)
          ], 
	  "Download data"))).

question(Tag, Code, Question, Response) -->
    { format(atom(Hash), '#~w', [Tag]) },
    html(form([class(form), method('POST'), action(Hash)],
      [ p(class('card-text'), Question),
        div(class("input-group mb-3"),
          [ div(class("input-group-prepend"), span(class("input-group-text"), "Response")),
            input([class("form-control"), type(text), name(Code), value(Response)]),
            div(class("input-group-append"),
                button([class('btn btn-primary'), type(submit)], "Submit"))
          ])
      ])).

navigation(Current, List) -->
    html(nav('aria-label'("Page navigation"),
        ul(class("pagination justify-content-center"),
          [ \navfirst(Current, List),
            \foreach(member(Number-X, List),
                navitem(Current, Number-X)),
            \navlast(Current, List)
          ]))).

navfirst(First, [_-First | _]) -->
    !, 
    html(li(class('page-item disabled'), a([class('page-link'), href(First)], "First"))).

navfirst(_, [_-First | _]) -->
    html(li(class('page-item'), a([class('page-link'), href(First)], "First"))).

navitem(Current, Number-Current) -->
    !, 
    html(li(class('page-item active'), a([class('page-link'), href(Current)], Number))).

navitem(_, Number-Link) -->
    html(li(class('page-item'), a([class('page-link'), href(Link)], Number))).

navlast(Last, List) -->
    { last(List, _-Last) },
    !, 
    html(li(class('page-item disabled'), a([class('page-link'), href(Last)], "Last"))).

navlast(_, List) -->
    { last(List, _-Last) },
    html(li(class('page-item'), a([class('page-link'), href(Last)], "Last"))).

ul_nonempty(_, []) -->
    [].

ul_nonempty(Title, [H | T]) -->
    { findall(li(X), member(X, [H | T]), Li) },
    html([ p(class('card-text'), Title), ul(class('card-text'), Li) ]).

cb_nonempty(_, _, []) -->
    [].

cb_nonempty(Title, Name, [H | T], Checked) -->
    { maplist([V-_, V-C] >> (member(V, Checked) -> C = [checked] ; C = []), [H | T], Codes),
      findall(
          li([ div(class('custom-control custom-checkbox'),
             [ input([class('custom-control-input'), type(checkbox), id(V), name(V) | C]),
               label([class('custom-control-label'), for(V)], V)
             ])
             % p(class('card-text'), L),
             % br('')
         ]), member(V-C, Codes), List) 
    },
    html(form([method('POST'), action('#question')],
      [ p(class('card-text'), Title), 
        ul(style('list-style: none; padding-left: 1em; text-indent: 0em;'), List),
        button([class('btn btn-secondary'), name(Name), value(upate)], "Update")
      ])).

