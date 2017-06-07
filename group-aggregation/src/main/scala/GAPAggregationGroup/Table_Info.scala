import Monoid.Monoid

// this is a util class for table..
class Table_Info[A](the_status:The_Status, level:Option[Int], weight:A)
{
  def get_status(): The_Status = {
    the_status
  }

  def get_level(): Option[Int] = {
    level
  }

  def get_weight(): A = {
    weight
  }
}
