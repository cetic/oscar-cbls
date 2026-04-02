package oscar.cbls.core.distributed.computation

/** Describes a task to execute.
  *
  * @param taskId
  *   an ID describing this instance of task. This ID is unique over the whole duration of the
  *   search procedure. It serves to kill a task, and to annotate task results when sent to the
  *   search procedure
  * @param taskClass
  *   an ID describing the class of the task. Each neighborhood that can be explored remotely has a
  *   different taskId There can be other tasks types, notably
  *   - in VLSN where a bunch of moves must be explored together
  *   - or in multi-criteria optimization where a task is a neighborhood plus some bounds to use in
  *     strong constraint
  * @param taskDescription
  *   the algorithmic data needed to perform a task. It must cast to the appropriate type
  *
  * NB: taskId and taskType are intentionally located here because a task will travel through a
  * bunch of different messages in the whole. To avoid useless copy-pasting they are kept together
  * here
  */
final case class Task(taskId: Long, taskClass: Int, taskDescription: TaskParameters)
