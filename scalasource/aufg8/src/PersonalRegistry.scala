import scala.collection.mutable.Map
/** A registry of personal in a department
  * 
  * @constructor creates a personal registry with given parameters
  * @param assignment Map[Department, Set[Personal]] is a Map of the departments linked to a set of personal within.
  * 
  */
class PersonalRegistry (assignment: scala.collection.mutable.Map[Department, Set[Personal]]):
    /**  Adds a person to a department in the personal registry
      *  @example:
      *  {{{
      *      assign(Person("Jane","Doe", nurse), physio)
      *  }}}
      *  @param  person      the Personal
      *  @param  department  the Department
      *  @return nothing mutates map in place
      */
    def assign(person: Personal, department: Department): Unit =
        val persons = personalIn(department) + person
        assignment(department) + person
    /** Retrieves assigned Personal in department
      *
      * @example
      * {{{
      *     personalIn(physio) // Set(Personal("Jane", "Doe", nurse))
      * }}}
      * @param  department      the Department
      * @return Set[Personal]   the Personal in that Department
      */
    def personalIn(department: Department): Set[Personal]= 
        assignment.getOrElse(department, Set.empty[Personal])
    /**
      * Returns a tuple of Sets of Personal sorted for PersonalType
      * @example
      * {{{
      *     typeOfPersonalIn(physio) // (Set(Personal("Jane", "Doe", nurse)), Set(Personal("John", "Doe", doctor)))
      * }}}
      * @param department  the Department
      * @return tuple of Sets of Personal
      */
    def typeOfPersonalIn(department: Department): (Set[Personal], Set[Personal]) =
        this.personalIn(department).partition(_.pType == PersonalType.doctor)