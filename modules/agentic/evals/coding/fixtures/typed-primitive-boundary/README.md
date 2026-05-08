Public boundary of a moderation service.

`ReportContent` is the entry point callers reach to file a moderation report.
Callers reach it directly from the gRPC handler.

The implementation stub validates none of its inputs. The task is to add the
input validation described in the doc comment.
