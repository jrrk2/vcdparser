open Gnuplot

let plot lst =
  let gp = Gp.create () in
  (* Plot lines and points. *)
  Gp.plot_many gp [ Series.points_xy lst ];
  Gp.close gp
