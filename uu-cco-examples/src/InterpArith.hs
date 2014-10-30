import CCO.Arith      (parser, eval)
import CCO.Component  (component, printer, ioWrap)
import Control.Arrow  ((>>>))

main = ioWrap (parser >>> component eval >>> printer)