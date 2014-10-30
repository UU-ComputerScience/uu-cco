import CCO.Arith      (Tm)
import CCO.Component  (Component, component, printer, ioWrap)
import CCO.Tree       (parser, Tree (toTree))
import Control.Arrow  ((>>>))

main = ioWrap $ parser >>> component toTree >>>
                (printer :: Component Tm String)