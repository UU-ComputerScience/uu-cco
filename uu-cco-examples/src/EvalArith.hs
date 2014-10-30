import CCO.Arith      (eval)
import CCO.Component  (component, printer, ioWrap)
import CCO.Tree       (parser, Tree (fromTree, toTree))
import Control.Arrow  (Arrow (arr), (>>>))

main = ioWrap $
  parser >>> component toTree >>> component eval >>> arr fromTree >>> printer