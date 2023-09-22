USE difxdb;

CREATE TABLE IF NOT EXISTS `ExperimentStatusHistory` (
  `id` bigint(20) unsigned NOT NULL,
  `expID` bigint(20) unsigned NOT NULL,
  `status` varchar(50) NOT NULL,
  `dateCreated` datetime NOT NULL
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

ALTER TABLE `ExperimentStatusHistory`
  ADD PRIMARY KEY (`id`),
  ADD KEY `expid` (`expID`);

ALTER TABLE `ExperimentStatusHistory`
  MODIFY `id` bigint(20) unsigned NOT NULL AUTO_INCREMENT;

ALTER TABLE `ExperimentStatusHistory`
  ADD CONSTRAINT `ExperimentStatusHistory_ibfk_1` FOREIGN KEY (`expID`) REFERENCES `Experiment` (`id`) ON DELETE CASCADE;

CREATE TRIGGER `insert_statushistory` AFTER INSERT ON `Experiment`
 FOR EACH ROW BEGIN
INSERT INTO ExperimentStatusHistory (expID, status, dateCreated) VALUES (NEW.id, (select experimentstatus from ExperimentStatus where NEW.statusID = id), NOW());
END

INSERT INTO `difxdb`.`ExperimentStatus` (`id`, `statuscode`, `experimentstatus`) VALUES (NULL, '60', 'Data released to PI');

INSERT INTO `difxdb`.`VersionHistory` (
`major` ,
`minor`
)
VALUES (
'1', '8'
);
