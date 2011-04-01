
class better_drawable ?colormap w pc = object 
  inherit GDraw.drawable ?colormap w

  val pango_context = (pc : GPango.context_rw)
  method pango_context = pango_context    

  method get_foreground = (Gdk.GC.get_values gc).Gdk.GC.foreground
  method get_background = (Gdk.GC.get_values gc).Gdk.GC.background
end
