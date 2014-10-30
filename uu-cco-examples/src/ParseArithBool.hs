import CCO.ArithBool  (parser)
import CCO.Component  (printer, ioWrap)
import CCO.Tree       (Tree (fromTree))
import Control.Arrow  (Arrow (arr), (>>>))

main = ioWrap (parser >>> arr fromTree >>> printer)