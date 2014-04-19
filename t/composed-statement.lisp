(in-package :cl-user)
(defpackage t.sxql.composed-statement
  (:use :cl
        :sxql
        :sxql.composed-statement
        :cl-test-more))
(in-package :t.sxql.composed-statement)

(plan 6)

(is (multiple-value-list
     (yield (compose-statements
             (select ((:+ 1 2))))))
    '("SELECT (? + ?)" (1 2)))

(is (multiple-value-list
     (yield (compose-statements
             (select :*
               (from :user)
               (where (:= :is_active 1)))
             (select :*
               (from :user)
               (where (:= :id 4))
               (limit 1)))))
    '("SELECT * FROM `user` WHERE (`is_active` = ?) AND (`id` = ?) LIMIT 1" (1 4)))

(is (multiple-value-list
     (yield (compose-statements
             (select :*
               (from :user)
               (where (:= :is_active 1))
               (limit 1))
             (select :config.language
               (left-join (:as :user_config :config)
                          :on (:= :config.user_id :user.id))))))
    '("SELECT `user`.*, `config`.`language` FROM `user` LEFT JOIN `user_config` AS `config` ON (`config`.`user_id` = `user`.`id`) WHERE (`user`.`is_active` = ?) LIMIT 1" (1)))

(is (multiple-value-list
     (yield (compose-statements
             (select :*
               (from :user)
               (where (:= :is_active 1))
               (limit 1))
             (select ((:as :config.profile :profile))
               (left-join (:as :user_config :config)
                          :on (:= :config.user_id :user.id))))))
    '("SELECT `user`.*, `config`.`profile` AS `profile` FROM `user` LEFT JOIN `user_config` AS `config` ON (`config`.`user_id` = `user`.`id`) WHERE (`user`.`is_active` = ?) LIMIT 1" (1)))

(is (multiple-value-list
     (yield (compose-statements
             (select :*
               (from :user)
               (where (:= :is_active 1))
               (limit 1))
             (select :config.*
               (left-join (:as :user_config :config)
                          :on (:= :config.user_id :user.id))))))
    '("SELECT `user`.*, `config`.* FROM `user` LEFT JOIN `user_config` AS `config` ON (`config`.`user_id` = `user`.`id`) WHERE (`user`.`is_active` = ?) LIMIT 1" (1)))

(defstruct custom-clause)

(defmethod yield ((clause custom-clause))
  "CUSTOM CLOUSE")

(is (multiple-value-list
     (yield (compose-statements
             (select :*
               (from :user)
               (make-custom-clause)
               (where (:= :is_active 1))
               (limit 1))
             (select :config.*
               (left-join (:as :user_config :config)
                          :on (:= :config.user_id :user.id))))))
    '("SELECT `user`.*, `config`.* FROM `user` CUSTOM CLOUSE LEFT JOIN `user_config` AS `config` ON (`config`.`user_id` = `user`.`id`) WHERE (`user`.`is_active` = ?) LIMIT 1" (1)))

(finalize)
