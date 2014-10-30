import CCO.ArithBool  (parser, checkTy, eval)
import CCO.Component  (component, printer, ioWrap)
import Control.Arrow  ((>>>))

main = ioWrap (parser >>> component (\t -> checkTy t >> eval t) >>> printer)