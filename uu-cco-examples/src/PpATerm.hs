import CCO.Component  (printer, ioWrap)
import CCO.Tree       (parser)
import Control.Arrow  ((>>>))

main = ioWrap (parser >>> printer)