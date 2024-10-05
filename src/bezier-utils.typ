#import "vector-utils.typ": *

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
  ((v0, _in-vec(vertex0), _sub(q0, v0)), (v1, _sub(r0, v1), _sub(r1, v1)), (v2, _sub(q2, v2), ..vertex1.slice(2)))
}

// Split any bezier at t
#let _split-bezier-t(t: 0.5, ..vertices) = {
  assert(vertices.named().len() == 0)
  let vertices = vertices.pos()
  vertices = vertices.zip(vertices.slice(1)).enumerate().map(((i, (v0, v1))) => _split-single-bezier(t, v0, v1)).sum()
  // Merge duplicate endpoints (may be a simpler way that I'm not thinking of)
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
  vertices
}

#let _offset-bezier(offset, ..vertices) = {
  // Note: this assumes no sharp corners
  assert(vertices.named().len() == 0)
  vertices.pos().map(
    v => {
      if _norm(_sub(..v.slice(1))).pt() == 0 {
        panic(v)
      }
      (
        _add(
          v.first(),
          _rot(_mult(_sub(_in-vec(v), _out-vec(v)), 1 / _norm(_sub(_in-vec(v), _out-vec(v))).pt() * offset.pt()), 90deg),
        ),
        ..v.slice(1),
      )
    },
  )
}

#let _make-cubic(..vertices) = {
  assert(vertices.named().len() == 0)
  let vertices = vertices.pos().map(v => if v.len() == 2 and type(v.first()) != array { (v,) } else { v })
  _roll(vertices, -1).zip(vertices, _roll(vertices, 1)).map(((v0, v1, v2)) => {
    let _calculate-control-points(v0, v1) = {
      let c0 = _out-vec(v0)
      let c1 = _in-vec(v1)
      if c0 == (0cm, 0cm) and c1 == (0cm, 0cm) {
        panic("TODO")
      } else if c0 == (0cm, 0cm) or c1 == (0cm, 0cm) {
        let c = if c1 == (0cm, 0cm) { _add(v0.first(), c0) } else { _add(v1.first(), c1) }
        c0 = _sub(_mult(_add(v0.first(), _mult(c, 2)), 1 / 3), v0.first())
        c1 = _sub(_mult(_add(v1.first(), _mult(c, 2)), 1 / 3), v1.first())
      }
      (c0, c1)
    }
    let (c0, c1) = _calculate-control-points(v0, v1)
    let (c2, c3) = _calculate-control-points(v1, v2)
    (v1.first(), c1, c2)
  })
}

#let _standardise-vertices(..vertices) = {
  assert(vertices.named().len() == 0)
  vertices.pos().map(_correct_vertex).map(v => (v.first(), _in-vec(v), _out-vec(v)))
}

#let reverse-path(..vertices) = {
  assert(vertices.named().len() == 0)
  vertices.pos().rev().map(v => if v.len() == 3 { (v.first(), ..v.slice(1).rev()) } else { (v.first(), _rot(v.last(), 180deg)) })
}

#let _shadow-bezier(shadow-radius: 0.5cm, shadow-stops: (gray, white), splits: 4, radius-multiplier: 1, ..vertices) = {
  // Convert vectors to and from relative values, for use with gradients
  let _frac(..vec) = vec.pos().map(x => x / 1cm * 100%)
  let _unfrac(..vec) = vec.pos().map(x => x * 1cm)

  // Split bezier into smaller parts to generate a better offset
  // This should be improved by splitting at sharpest edges or least "circular" points
  let bezier = _split-bezier-rep(splits, ..vertices)
  for (v0, v1) in bezier.zip(bezier.slice(1)) {
    (v0, v1) = _make-cubic(v0, v1)
    v0.at(1) = (0cm, 0cm)
    v1.at(2) = (0cm, 0cm)
    let vs = (v0, v1)
    // Create the outer edge of the shadow. Multiplying it by some factor reduces artefacts from cropped edges
    let vs = reverse-path(.._offset-bezier(radius-multiplier * shadow-radius, ..vs))
    vs = (v0, v1, ..vs)
    vs = vs.map(v => if v.len() == 3 { v } else { (..v, _rot(v.last(), 180deg)) })

    //Make the corners of the shadow segments square
    vs = vs.enumerate().map(((i, v)) => (
      v.first(),
      if calc.rem(i, 2) == 1 { v.at(1) } else { (0pt, 0pt) },
      if calc.rem(i, 2) == 0 { v.at(2) } else { (0pt, 0pt) },
    ))

    // Calculate angle change from start to finish
    let theta = calc.atan2(..v1.at(1).map(x => x.pt())) - calc.atan2(..v0.last().map(x => x.pt())) + 180deg
    let theta = calc.rem(theta.deg(), 360) * 1deg

    // Calculate radii using logic from here: https://stackoverflow.com/questions/1734745/how-to-create-circle-with-b%C3%A9zier-curves
    let factor = 4 / 3 * calc.tan(theta / 4)
    let r0 = _norm(_out-vec(v0)) / factor
    let r1 = _norm(_in-vec(v1)) / factor

    // Calculate the centre of the shadow from both edges, then get the midpoint.
    let s0 = _add(v0.first(), _rot((r0, 0cm), calc.atan2(.._rot(_out-vec(v0), 90deg).map(x => x.pt()))))
    let s1 = _add(v1.first(), _rot((r1, 0cm), calc.atan2(.._rot(_in-vec(v1), -90deg).map(x => x.pt()))))
    let s = _add(s0, _mult(_sub(s1, s0), 0.5))

    // Calculate the average radius. Note this is signed because that indicates convexity
    let r = (r0 + r1) / 2

    if r0 == 0cm {
      r = r1
      s = s1
    } else if r1 == 0cm {
      r = r0
      s = s0
    }

    // Draw the segment of shadow. Putting it in a square box ensures
    // the gradient will be circular. Otherwise it will depend on the
    // global location of the segment. Note that effect could be
    // improved by making use of elliptical shadows, along with the
    // focal centre, since the values of the two radii are different
    place(box(width: 1cm, height: 1cm, path(stroke: none, fill: gradient.radial(
      ..if r.pt() > 0 { shadow-stops } else { shadow-stops.rev() },
      center: _frac(..s),
      radius: _frac(calc.abs(r) + if r.pt() > 0 { shadow-radius } else { 0cm }).first(),
      focal-radius: _frac(calc.abs(r) - if r.pt() < 0 { shadow-radius } else { 0cm }).first(),
      relative: "parent",
    ), closed: true, ..vs)))
  }
  for (v0, v1, v2) in vertices.pos().zip(vertices.pos().slice(1), vertices.pos().slice(2)) {
    if _in-vec(v0) != _rot(_out-vec(v0), 180deg) {
      (v0, v1) = _make-cubic(v0, v1)
      (v1, v2) = _make-cubic(v1, v2)

      let in_angle = calc.atan2(.._in-vec(v1).map(x => x.pt()))
      let out_angle = calc.atan2(.._out-vec(v1).map(x => x.pt()))
      let theta = out_angle - in_angle + 180deg
      let cp_size = 4 / 3 * calc.tan(theta / 4) * shadow-radius * radius-multiplier

      place(
        box(
          width: 1cm,
          height: 1cm,
          path(
            closed: true,
            (v1.first()),
            (
              _add(v1.first(), _rot((shadow-radius * radius-multiplier, 0cm), in_angle + 90deg)),
              (0cm, 0cm),
              _rot((cp_size, 0cm), in_angle + 180deg),
            ),
            (
              _add(v1.first(), _rot((shadow-radius * radius-multiplier, 0cm), out_angle - 90deg)),
              _rot((cp_size, 0cm), out_angle + 180deg),
              (0cm, 0cm),
            ),
            fill: gradient.radial(..shadow-stops, relative: "parent", radius: _frac(shadow-radius).first(), center: _frac(..v1.first())),
          ),
        ),
      )
    }
  }
}

#_shadow-bezier(((1cm, 1cm), (-6cm, 0cm), (20cm, 1cm)), ((5cm, 5cm), (0cm, 0cm), (-5cm, -5cm)), ((8cm, 8cm), (0cm, -6cm)))
// #_shadow-bezier(((1cm, 1cm), (-6cm, 0cm), (20cm, 1cm)), ((5cm, 5cm), (0cm, 0cm), (-5cm, 0cm)), ((8cm, 8cm), (0cm, -6cm)))
// #path(
//   stroke: black,
//   ((1cm, 1cm), (-6cm, 0cm), (20cm, 1cm)),
//   ((5cm, 5cm), (0cm, 0cm), (-5cm, 0cm)),
//   ((8cm, 8cm), (0cm, -6cm)),
// )

// #_shadow-bezier(((1cm, 1cm), (-6cm, 0cm), (6cm, 0cm)), ((8cm, 8cm), (0cm, -6cm)))
// #_shadow-bezier(((1cm, 1cm), (-6cm, 0cm), (20cm, 1cm)), ((8cm, 8cm), (0cm, -6cm)))

// #square(height: 1cm, width: 10cm)