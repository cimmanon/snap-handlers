This is a collection of handlers I tend to reuse from one Snap project to the next.  Everything here is considered experimental and subject to change at any point in time.  Included are handlers for dealing with the following:

* Dealing with param information (casting them to integers, etc.)
* Form processing with Digestive Functors (including Digestive Functors Aeson)
* HTTP error responses (including adding a wrapper to hide ugly errors behind a pretty error page in production mode)

The snap-extras package is an unfortunate dependency at this point in time.  I'd like to see about removing this.