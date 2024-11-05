
#set page(
  paper: "a4",
)
#set par(justify: true)

= Response Letter

#v(1cm)
#[
#set text(size:13pt)

We would like to thank the reviewers for their constructive feedback regarding
the writing issues in our manuscript. We have made the necessary revisions, and
our detailed responses are as follows:
]

#v(.2cm)
== Reviewer \#1

#v(.2cm)
- #[First of all, I find the way the authors marked the changes in this revision
compared to the first one a bit annoying (following the Track Changes feature
of Microsoft Word). It is not necessary to keep the old text, and mark it with
the red color, and strikethrough font. I would just remove the old text, and
mark the new one with the blue color.]
- #[In Pages 6, 7, 10 there are some missing references, i.e., "Figure ??" please
fix this.]

#[
#set list(marker:[>])
- #[
The missing reference issues stem from the tool `latex-diff`, which we used to
automatically highlight changes in the previous revision. Consequently, these
issues appeared only in the file that marked the changes and not in the final
revision. 
In this new revision, we have manually marked the changes instead of relying on
the tool.
]
]

#v(.7cm)
- #[There is a typo in Page 31: "colleced"]

#[
#set list(marker:[>])
- #[
Thanks for the feedback. We fixed the typo.
]
]
