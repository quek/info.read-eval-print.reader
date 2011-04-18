(in-package :info.read-eval-print.reader)

(unless rucksack:*rucksack*
  (setf rucksack:*rucksack*
        (rucksack:open-rucksack (merge-pathnames "rucksack/" *default-directory*))))
