#import "@preview/umbra:0.1.0": shadow-path

#let backgroud-colour = color.rgb("#EFEEEE")
#let radius = 0.4cm

#set page(width: 15cm, height: 15cm, margin: 0.5cm, fill: backgroud-colour)

#box(
  {
    place(shadow-path(
      shadow-stops: (white.mix((backgroud-colour, 50%)), backgroud-colour,),
      shadow-radius: radius,
      (16%, 15%),
      (15%, 16%),
      (15%, 79%),
      (16%, 80%),
      (79%, 80%),
      (80%, 79%),
      (80%, 16%),
      (79%, 15%),
      closed: true,
    ))
    place(
      shadow-path(
        shadow-stops: (color.rgb("#D1CDC7").mix((backgroud-colour, 50%)), backgroud-colour,),
        shadow-radius: radius,
        (21%, 20%),
        (20%, 21%),
        (20%, 84%),
        (21%, 85%),
        (84%, 85%),
        (85%, 84%),
        (85%, 20%),
        (84%, 21%),
        closed: true,
      ),
    )
    polygon(
      fill: backgroud-colour,
      (16%, 15%),
      (15%, 16%),
      (15%, 84%),
      (16%, 85%),
      (85%, 84%),
      (84%, 85%),
      (85%, 16%),
      (84%, 15%),
    )
  },
)