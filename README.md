## westbrook

This project is for generating RSS feeds.

It is used by creating a westbrook:feed object, adding westbrook:item
objects to the feed, then using westbrook:generate or
westbrook:generate-to to write the XML feed to a string or file.

I use it to generate feeds for Quicklisp build failure reports.

### Example

    (let ((feed (make-instance 'feed
                               :title "My Great Feed"
			       :link "https://www.xach.com/"
			       :description "My feed.")))
      (add-feed-item feed
      		     :title "An item"
		     :description "Here is <a href='https://github.com/xach/vecto/'>vecto</a>!"
		     :guid "my-great-feed-item-1"
		     :guid-permalink-p nil)
      (generate-to feed "rss.xml"))

### Feedback

Development of westbrook takes place on
[github](https://github.com/xach/westbrook/). If you find a bug, please
report via [github
issues](https://github.com/xach/westbrook/issues). You can also email
me at [xach@xach.com](mailto:xach@xach.com).

Enjoy!

