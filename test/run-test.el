(require 'ert)
(require 'slack-team)
(require 'slack-channel)
(require 'slack-usergroup)
(require 'slack-message-formatter)
(require 'slack-block)
(defvar slack-channel-button-keymap nil)

(defmacro slack-test-setup (&rest body)
  (declare (indent 0) (debug t))
  `(let* ((channel-id "C11111")
          (channel-name "TestChannel")
          (channel (make-instance 'slack-channel
                                  :id channel-id
                                  :name channel-name))
          (user-id "U11111")
          (user-name "TestUser")
          (user (list :name user-name :id user-id))
          (usergroup-id "S88888")
          (usergroup-handle "TestUsergroup")
          (usergroup (make-instance 'slack-usergroup
                                    :id usergroup-id
                                    :handle usergroup-handle))
          (team (make-instance 'slack-team
                               :channels (list channel)
                               :users (list user)
                               :usergroups (list usergroup))))
     ,@body))

(ert-deftest slack-test-unescape-&<> ()
  (should (equal "<" (slack-unescape-&<> "&lt;")))
  (should (equal ">" (slack-unescape-&<> "&gt;")))
  (should (equal "&" (slack-unescape-&<> "&amp;")))
  (should (equal "foo" (slack-unescape-&<> "foo"))))

(ert-deftest slack-test-unescape-channel ()
  (slack-test-setup
    (should (equal (format "#%s" channel-name)
                   (slack-unescape-channel
                    (format "<#%s>" channel-id)
                    team)))
    (should (equal "#Foo"
                   (slack-unescape-channel
                    (format "<#%s|Foo>" channel-id)
                    team)))
    (should (equal "#<Unknown CHANNEL>"
                   (slack-unescape-channel
                    "<#C9999999>" team)))))

(ert-deftest slack-test-unescape-@ ()
  (slack-test-setup
    (should (equal (format "@%s" user-name)
                   (slack-unescape-@
                    (format "<@%s>" user-id)
                    team)))
    (should (equal "@Foo"
                   (slack-unescape-@
                    (format "<@%s|Foo>" user-id)
                    team)))
    (should (equal "@<Unknown USER>"
                   (slack-unescape-@
                    "<@U424242>" team)))))

(ert-deftest slack-test-unescape-!subteam ()
  (slack-test-setup
    (should (equal (slack-unescape-!subteam
                    (format "<!subteam^%s|@%s>"
                            usergroup-id
                            usergroup-handle))
                   (format "@%s" usergroup-handle)))))


(ert-deftest slack-test-unescape-!date ()
  (should (equal (slack-unescape-!date
                  "<!date^1392734382^Posted {date_num} {time_secs}|Posted 2014-02-18 14:39:42>"
                  0)
                 "Posted 2014-02-18 14:39:42") )
  (should (equal (slack-unescape-!date
                  "<!date^1392734382^{date} at {time}|February 18 2014 at 14:39 PST>"
                  0)
                 "February 18, 2014 at 14:39"))
  (should (equal (slack-unescape-!date
                  "<!date^1392734382^{date_short}^https://example.com/|Feb 18, 2014 PST>"
                  0)
                 "<https://example.com/|Feb 18, 2014>"))
  )


(ert-deftest slack-test-unescape-variable ()
  (should (equal "@here" (slack-unescape-variable "<!here>")))
  (should (equal "@here" (slack-unescape-variable "<!here|here>")))
  (should (equal "@channel" (slack-unescape-variable "<!channel>")))
  (should (equal "@everyone" (slack-unescape-variable "<!everyone>")))
  (should (equal "<foo>" (slack-unescape-variable "<!foo>")))
  (should (equal "<label>" (slack-unescape-variable "<!foo|label>"))))

(ert-deftest slack-test-section-layout-block ()
  (let* ((input '(:type "section"
                        :block_id "2XEH1"
                        :text (:type "mrkdwn"
                                     :text "Hello, Assistant to the Regional Manager Dwight! *Michael Scott* wants to know where you'd like to take the Paper Company investors to dinner tonight.\n\n *Please select a restaurant:*"
                                     :verbatim
                                     :json-false)))
         (out (slack-create-layout-block input))
         (text (oref out text)))
    (should (eq 'slack-section-layout-block
                (eieio-object-class-name out)))
    (should (equal "2XEH1" (oref out block-id)))
    (should (eq (eieio-object-class-name text)
                'slack-text-message-composition-object))))

(ert-deftest slack-test-divider-layout-block ()
  (let* ((input '(:type "divider" :block_id "y5a"))
         (out (slack-create-layout-block input)))
    (should (eq 'slack-divider-layout-block
                (eieio-object-class-name out)))
    (should (equal (oref out block-id)
                   "y5a"))))

(ert-deftest slack-test-image-layout-block ()
  (let* ((input '(:type "image" :block_id "Tron" :image_url "https://api.slack.com/img/blocks/bkb_template_images/beagle.png" :alt_text "image1" :title (:type "plain_text" :text "image1" :emoji t) :fallback "1080x1080px image" :image_width 1080 :image_height 1080 :image_bytes 1432686))
         (out (slack-create-layout-block input)))
    (should (eq 'slack-image-layout-block
                (eieio-object-class-name out)))
    (should (eq 1080 (oref out image-width)))
    (should (eq 1080 (oref out image-height)))
    (should (equal
             "https://api.slack.com/img/blocks/bkb_template_images/beagle.png"
             (oref out image-url)))

    (should (equal "image1" (oref (oref out title) text)))
    (should (eq t (oref (oref out title) emoji)))
    ))

(ert-deftest slack-test-actions-layout-block ()
  (let* ((input '(:type
                  "actions"
                  :block_id "lzhj"
                  :elements ((:type
                              "conversations_select"
                              :action_id "r4Y"
                              :placeholder (:type
                                            "plain_text"
                                            :text "Select a conversation"
                                            :emoji t)))))
         (out (slack-create-layout-block input)))
    (should (eq 'slack-actions-layout-block
                (eieio-object-class-name out)))
    (should (eq 1 (length (oref out elements))))
    (should (equal "lzhj" (oref out block-id)))
    ))

(ert-deftest slack-text-context-layout-block ()
  (let* ((input '(:type "context" :block_id "mOfwN" :elements ((:type "plain_text" :text "Last updated: Jan 1, 2019" :emoji t) (:fallback "600x800px image" :image_url "https://api.slack.com/img/blocks/bkb_template_images/goldengate.png" :image_width 600 :image_height 800 :image_bytes 828593 :type "image" :alt_text "goldengate"))))
         (out (slack-create-layout-block input)))
    (should (eq 'slack-context-layout-block
                (eieio-object-class-name out)))
    (should (equal "mOfwN" (oref out block-id)))
    (should (eq 2 (length (oref out elements))))
    ))

(if noninteractive
    (ert-run-tests-batch-and-exit)
  (ert t))

