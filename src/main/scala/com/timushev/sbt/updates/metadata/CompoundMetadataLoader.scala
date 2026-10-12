package com.timushev.sbt.updates.metadata

import com.timushev.sbt.updates.LookupSession.executionContext
import com.timushev.sbt.updates.versions.Version
import sbt.{Logger, ModuleID}

import java.io.FileNotFoundException
import scala.concurrent.Future
import scala.util.control.NonFatal
import scala.util.{Failure, Success}

/** Adds the versions of a directory listing to those of the Maven metadata, which can be stale (e.g. it lists no 2.0.x
  * versions of sbt-scalafmt on Maven Central). Many repositories serve no listings, so a lookup fails only when the
  * metadata could not be read and the listing has nothing to make up for it.
  */
class CompoundMetadataLoader(val repository: String, metadata: MetadataLoader, listing: MetadataLoader, logger: Logger)
    extends MetadataLoader {
  override def getVersions(module: ModuleID): Future[Seq[Version]] = {
    val listed = listing.getVersions(module).map(Option(_)).recover { case NonFatal(_) => None }
    metadata.getVersions(module).transform(Success(_)).zipWith(listed) {
      case (Success(versions), another)              => versions ++ another.getOrElse(Nil)
      case (Failure(_: FileNotFoundException), None) => throw new FileNotFoundException(s"$module in $repository")
      case (Failure(_: FileNotFoundException), Some(found)) => found
      case (Failure(cause), Some(found)) if found.nonEmpty  =>
        logger.debug(
          s"$repository: using the directory listing of $module, as its Maven metadata can't be read: $cause"
        )
        found
      case (Failure(cause), _) => throw cause
    }
  }
}
