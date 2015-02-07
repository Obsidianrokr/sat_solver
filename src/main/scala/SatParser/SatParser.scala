package SatParser

import SatModel.SatEquation

/**
 * Created by vladimir on 1/20/15.
 */
trait SatParser {
  def parse(lines: Iterator[String]): SatEquation
}
