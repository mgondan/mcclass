:- module(tpaired, [item//2]).

:- use_module(library(http/html_write)).
:- multifile item//2.

item(tpaired, Data) -->
    { D = 4.5,
      Mu = 3.0,
      S_D = 8.2,
      N = 33,
      T0 = 23.2,
      S_T0 = 11.7,
      EOT = 17.7,
      S_EOT = 13.2,
      Tails = "two-tailed",
      Alpha = 0.05,
      option(response(Response), Data, '#.##')
    },
    html(
      [ div(class(card), div(class('card-body'),
          [ h1(class('card-title'), "Phase II clinical study"),
            p(class('card-text'),
            [ "Consider a clinical study on rumination-focused Cognitive ",
              "Behavioral Therapy (rfCBT) with ",
              math(mrow([mi('N'), mo(=), mn(N)])),
              " patients. The primary outcome is the score on the ",
              "Hamilton Rating Scale for Depression (HDRS, range from ",
              "best = 0 to worst = 42). The significance level is set to ",
              math(mrow([mi(&(alpha)), mo(=), mn(Alpha)])),
              "two-tailed."])
          ])),
        div(class(card), div(class('card-body'),
          [ h4(class('card-title'), [a(id(question), []), "Question"]),
            p(class('card-text'),
              [ "Does rfCBT lead to a relevant reduction (i.e., more than ",
	        math(mrow([mi(&(mu)), mo(=), mn(Mu)])),
                " units) in mean HDRS scores between ",
                "baseline (T0) and End of Treatment (EOT)?"
              ]),
            form([class(form), method('POST'), action('#tpaired-tratio')],
              [ p(class('card-text'), "Question"),
                div(class("input-group mb-3"),
                  [ div(class("input-group-prepend"), span(class("input-group-text"), "Response")),
                      input([class("form-control"), type(text), name(response), value(Response)]),
                    div(class("input-group-append"),
                      button([class('btn btn-primary'), type(submit)], "Submit"))
              ])])
          ]))
      ]).
