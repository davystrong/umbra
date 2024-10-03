// Gets the length of an arbitrary vector
#let _norm(p) = calc.sqrt(p.map(x => calc.pow(x.pt(), 2)).sum()) * 1pt
// Adds any number of arbitrary vectors
#let _add(..ps) = ps.pos().fold(none, (acc, x) => if acc == none { x } else { acc.zip(x).map(((y, z)) => y + z) })
// Takes the first vector and subtracts all subsequent vectors from it
#let _sub(..ps) = ps.pos().fold(none, (acc, x) => if acc == none { x } else { acc.zip(x).map(((y, z)) => y - z) })
// Rotates a 2D vector by the given angle
#let _rot(p, angle) = (
  p.first() * calc.cos(angle) - p.last() * calc.sin(angle),
  p.first() * calc.sin(angle) + p.last() * calc.cos(angle),
)
// Multiply (scale) a vector by some number
#let _mult(p, x) = p.map(y => x * y)
// Roll a vector by count positions, moving the overflow at the end back to the start
#let _roll(arr, count) = (arr.slice(count) + arr).slice(0, arr.len())

#let shadow-path(
  fill: none,
  stroke: none,
  closed: false,
  shadow-radius: 0.5cm,
  shadow-stops: (gray, white),
  // A small correction is required otherwise there is a white line between shadow sections
  correction: 5deg,
  ..vertices,
) = {
  let vertices = vertices.pos()
  assert(
    vertices.all(x => x.len() == 2 and x.all(y => type(y) != array)),
    message: "paths with Bezier control points not supported",
  )
  layout(
    size => {
      let vertices = vertices.map(
        ((x, y)) => (if type(x) == ratio { x * size.width } else { x }, if type(y) == ratio { y * size.height } else { y },),
      )

      let groups = vertices.zip(_roll(vertices, 1), _roll(vertices, 2), _roll(vertices, 3))
      if not closed {
        groups = _roll(groups, -1).slice(0, -1)
      }

      // Setup edge shadows
      for (p0, p1, p2, p3) in groups {
        let angle0 = calc.atan2(.._sub(p1, p0).map(x => x.pt()))
        angle0 += if angle0 > 0deg { 0deg } else { 360deg }
        let angle1 = calc.atan2(.._sub(p2, p1).map(x => x.pt()))
        angle1 += if angle1 > 0deg { 0deg } else { 360deg }
        let angle2 = calc.atan2(.._sub(p3, p2).map(x => x.pt()))
        angle2 += if angle2 > 0deg { 0deg } else { 360deg }

        let width = shadow-radius
        let height = _norm(_sub(p1, p2))
        let d0 = 0pt
        let d1 = 0pt

        let da0 = angle1 - angle0
        let da1 = angle2 - angle1
        if da0 < 0deg or da0 > 180deg {
          da0 = 0
        }
        if da1 < 0deg or da1 > 180deg {
          da1 = 0
        }
        place(
          top + left,
          dx: p2.first(),
          dy: p2.last(),
          rotate(calc.atan2(.._sub(p1, p2).map(x => x.pt())) + 90deg + 180deg, origin: top + left, polygon(
            fill: gradient.linear(..shadow-stops),
            (0pt, 0pt),
            _rot((width, 0pt), da1 / 2),
            _add((0pt, height), _rot((width, 0pt), -da0 / 2)),
            (0pt, height),
          )),
        )
      }

      // Setup corner shadows
      if not closed {
        groups = groups.slice(1)
      }

      for (p0, p1, p2, p3) in groups {
        let angle0 = calc.atan2(.._sub(p1, p0).map(x => x.pt()))
        angle0 += if angle0 > 0deg { 0deg } else { 360deg }
        let angle1 = calc.atan2(.._sub(p2, p1).map(x => x.pt()))
        angle1 += if angle1 > 0deg { 0deg } else { 360deg }

        let da = angle1 - angle0
        if da < 0deg or da > 180deg {
          da = calc.abs(da)
          let d0 = _rot((shadow-radius, 0pt), angle0 + 90deg + correction)
          let d1 = _rot((shadow-radius, 0pt), angle1 + 90deg - correction)
          // Must be placed in the correct location, otherwise the gradient is based on the size of the whole box
          place(
            top + left,
            dx: p1.first() - shadow-radius,
            dy: p1.last() - shadow-radius,
            box(
              // A fixed size box is required to make radial gradient work. For PDF, the gradient doesn't actually have to be contained by the box, but this breaks with PNG, hence the extra complexity
              width: 2 * shadow-radius,
              height: 2 * shadow-radius,
              //TODO: Switch to this logic: https://stackoverflow.com/questions/1734745/how-to-create-circle-with-b%C3%A9zier-curves
              place(top + left, dx: 50%, dy: 50%, path(
                closed: true,
                fill: gradient.radial(..shadow-stops, center: (50%, 50%), radius: 50%, relative: "parent"),
                (d0, _mult(_rot(d0, -90deg), calc.sin(da / 2)), (0pt, 0pt)),
                (0pt, 0pt),
                ((d1), (0pt, 0pt), _mult(_rot(d1, 90deg), calc.sin(da / 2)),),
              )),
            ),
          )
        }
      }

      if fill != none or stroke != none {
        path(fill: fill, stroke: stroke, closed: closed, ..vertices)
      }
    },
  )
}

#let shadow-circle(
  radius: 0pt,
  width: auto,
  height: auto,
  fill: none,
  stroke: none,
  inset: 5pt,
  outset: (:),
  shadow-radius: 0.5cm,
  shadow-stops: (gray, white),
  // A small correction factor to avoid a line between the shadow and the fill
  correction: 0.00001%,
  ..body,
) = {
  assert(
    (radius, width, height).filter(it => it == 0pt or it == auto or it == none).len() >= 2,
    message: "radius, width and height are mutually exclusive",
  )
  layout(
    size => {
      // Replicate the built in optional positional body argument
      assert(body.named().len() == 0 and body.pos().len() <= 1, message: "unexpected argument")
      let body = if body.pos().len() == 1 { body.pos().first() } else { none }

      // Width and height seem to only be to allow for sizing relative to parent
      let radius = radius
      if not (width == 0pt or width == auto or width == none) {
        if type(width) == ratio {
          radius = width * size.width
        } else {
          radius = width
        }
      } else if not (height == 0pt or height == auto or height == none) {
        if type(height) == ratio {
          radius = height * size.height
        } else {
          radius = height
        }
      }

      // Avoid an unnecessary place
      let inner = circle(
        radius: radius + shadow-radius,
        fill: gradient.radial(
          // Making it transparent doesn't actually do anything yet since gradients
          // can't handle transparency
          // Might be better to do this with a focal radius
          (shadow-stops.last().transparentize(100%), 0%),
          (shadow-stops.last().transparentize(100%), radius / (radius + shadow-radius) * 100% - correction,),
          ..shadow-stops.enumerate().map(
            ((i, stop)) => (stop, (radius + shadow-radius * i / (shadow-stops.len() - 1)) / (radius + shadow-radius) * 100%,),
          ),
        ),
        stroke: none,
        inset: inset,
        outset: outset,
      )

      if fill != none or stroke != none or body != none {
        place(inner)
        place(
          dx: shadow-radius,
          dy: shadow-radius,
          circle(radius: radius, fill: fill, stroke: stroke, inset: inset, outset: outset, body),
        )
      } else {
        inner
      }
    },
  )
}

#let _correct_vertex(vertex) = {
  if vertex.len() == 1 {
    (..vertex, (0cm, 0cm))
  } else if vertex.len() >= 2 {
    if type(vertex.first()) == array {
      vertex
    } else {
      (vertex, (0cm, 0cm))
    }
  } else {
    panic("invalid vertex", vertex)
  }
}

#let _in-vec(vertex) = vertex.at(1)

#let _out-vec(vertex) = if vertex.len() == 3 {
  vertex.last()
} else {
  _rot(vertex.last(), 180deg)
}

// Split a bezier curve defined by two vertices at t
#let _split-single-bezier(t, vertex0, vertex1) = {
  // See https://en.wikipedia.org/wiki/B%C3%A9zier_curve#Higher-order_curves
  // for a good diagram for this. Some of the naming is shared
  // Basic vertex validation and correction
  assert(t >= 0 and t <= 1)
  let vertex0 = _correct_vertex(vertex0)
  let vertex1 = _correct_vertex(vertex1)

  // Calculate the new control points
  let v0 = vertex0.first()
  let v2 = vertex1.first()
  let q0 = _add(v0, _mult(_out-vec(vertex0), t))
  let q2 = _add(v2, _mult(_in-vec(vertex1), 1 - t))
  let q1 = _add(v0, _out-vec(vertex0), _mult(_sub(_add(v2, _in-vec(vertex1)), _add(v0, _out-vec(vertex0))), t))
  let r0 = _add(q0, _mult(_sub(q1, q0), t))
  let r1 = _add(q1, _mult(_sub(q2, q1), t))
  let v1 = _add(r0, _mult(_sub(r1, r0), t))

  // Compile the three new vertices
  let a = (q2, v2)
  let temp = _sub(q2, v2)
  ((v0, vertex0.at(1), _sub(q0, v0)), (v1, _sub(r0, v1), _sub(r1, v1)), (v2, _sub(q2, v2), ..vertex1.slice(2)))
}

// Split any bezier at t
#let _split-bezier-t(t: 0.5, ..vertices) = {
  assert(vertices.named().len() == 0)
  let vertices = vertices.pos()
  vertices = vertices.zip(vertices.slice(1)).enumerate().map(((i, (v0, v1))) => _split-single-bezier(t, v0, v1)).sum()
  // Merge duplicate endpoints (may be a simpler way)
  vertices.zip(vertices.slice(1) + (none,)).enumerate().map(((i, (v0, v1))) => {
    if calc.rem(i, 3) == 2 and v1 != none {
      (..v0.slice(0, 2), v1.last())
    } else if i != 0 and calc.rem(i, 3) == 0 and v1 != none {
      none
    } else {
      v0
    }
  }).filter(it => it != none)
}

// Recursively split any bezier at 0.5
#let _split-bezier-rep(rep, ..vertices) = {
  assert(vertices.named().len() == 0)
  let vertices = vertices.pos()
  for _ in range(rep) {
    vertices = _split-bezier-t(..vertices)
  }
  // panic(vertices)
  vertices
}

#let _offset-bezier(offset, ..vertices) = {
  // Note: this assumes no sharp corners
  assert(vertices.named().len() == 0)
  vertices.pos().map(v => {
    if _norm(_sub(..v.slice(1))).pt() == 0 {
      panic(v)
    }
    (
      _add(v.first(), _rot(_mult(_sub(..v.slice(1)), 1 / _norm(_sub(..v.slice(1))).pt() * offset.pt()), 90deg)),
      ..v.slice(1),
    )
  })
}

#let _standardise-vertices(..vertices) = {
  assert(vertices.named().len() == 0)
  vertices.pos().map(_correct_vertex).map(v => if v.len() == 3 { v } else { (v.first(), v.last(), _rot(v.last(), 180deg)) })
}

#let reverse-path(..vertices) = {
  assert(vertices.named().len() == 0)
  vertices.pos().rev().map(v => if v.len() == 3 { (v.first(), ..v.slice(1).rev()) } else { (v.first(), _rot(v.last(), 180deg)) })
}

#let _shadow-bezier(shadow-radius: 0.5cm, shadow-stops: (gray, white), vertex0, vertex1) = {
  // (vertex0, vertex1) = _standardise-vertices(vertex0, vertex1)
  // Convert vectors to and from relative values, for use with gradients
  let _frac(vec) = vec.map(x => x / 1cm * 100%)
  let _unfrac(vec) = vec.map(x => x * 1cm)
  //TODO: Convert bilinear curves (missing a control point) to bicubic
  place(path(vertex0, vertex1))
  // panic(_split-bezier-t(t: 0.5, vertex0, vertex1))
  // place(path(stroke: green, .._split-bezier-t(.._split-bezier-t(t: 0.5, vertex0, vertex1))))
  // place(path(stroke: green, .._split-bezier-t(t: 0.5, vertex0, vertex1)))
  place(path(stroke: yellow, .._split-bezier-rep(2, vertex0, vertex1)))
  place(path(stroke: green, .._split-bezier-rep(6, vertex0, vertex1)))
  place(path(stroke: blue, .._offset-bezier(shadow-radius, .._split-bezier-rep(5, vertex0, vertex1))))
  // panic(vertex0, vertex1)
  let bezier = _split-bezier-rep(3, vertex0, vertex1)
  // panic(bezier)
  for (v0, v1) in bezier.zip(bezier.slice(1)) {
    let vs = (v0, v1)
    // Create the edge of the shadow
    let vs = reverse-path(.._offset-bezier(shadow-radius, ..vs))
    vs = (v0, v1, ..vs)
    vs = vs.map(v => if v.len() == 3 { v } else { (..v, _rot(v.last(), 180deg)) })
    //Make the corners of the shadow segments square
    vs = vs.enumerate().map(((i, v)) => (
      v.first(),
      if calc.rem(i, 2) == 1 { v.at(1) } else { (0pt, 0pt) },
      if calc.rem(i, 2) == 0 { v.at(2) } else { (0pt, 0pt) },
    ))

    let bbox = vs.fold((0pt, 0pt), (acc, v) => {
      acc.zip(v.first()).map(((av, vv)) => calc.max(av, vv))
    })

    let theta = calc.atan2(..v1.at(1).map(x => x.pt())) - calc.atan2(..v0.last().map(x => x.pt()))
    // panic(theta)
    // place(dx: bbox.first(), dy: bbox.last(), text(str(theta.deg())))
    // place(dx: bbox.first(), dy: bbox.last(), text(str(v0.len())))

    bbox = _frac(bbox)
    place(box(width: 1cm, height: 1cm, path(
      stroke: red,
      fill: gradient.radial(green, blue, red, yellow, center: bbox, radius: 50%, relative: "parent"),
      closed: true,
      ..vs,
    )))
    // place(path((0pt, 0pt), _unfrac(bbox)))
  }
}

// #_shadow-bezier(((2cm, 2cm),), ((8cm, 8cm), (0cm, -6cm)))
#_shadow-bezier(((1cm, 1cm), (-6cm, 0cm), (6cm, 0cm)), ((8cm, 8cm), (0cm, -6cm)))

// #circle(radius: 5cm, fill: gradient.radial(focal-radius: 40%, blue, yellow))
// #ellipse(height: 2cm, width: 10cm, fill: gradient.radial(focal-radius: 40%, blue, yellow))