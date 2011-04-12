
class better_drawable ?colormap w pc = 
object (self)
  inherit GDraw.drawable ?colormap w

  val pango_context = (pc : GPango.context_rw)
  method pango_context = pango_context    

  method get_foreground = (Gdk.GC.get_values gc).Gdk.GC.foreground
  method get_background = (Gdk.GC.get_values gc).Gdk.GC.background

end


let error_message_dialog message =
  let err = GWindow.message_dialog ~message
    ~message_type:`ERROR
    ~buttons:GWindow.Buttons.ok () 
  in
  (* ignore(err#connect#response ~callback:(fun _ -> err#destroy())); *)
  ignore(err#connect#response ~callback:(fun _ -> exit 1));
  err#show()

