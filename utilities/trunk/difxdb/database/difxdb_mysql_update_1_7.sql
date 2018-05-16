USE difxdb;

CREATE TABLE IF NOT EXISTS `FileData` (
  `id` bigint(20) unsigned NOT NULL,
  `experimentID` bigint(20) unsigned NOT NULL,
  `location` varchar(255) DEFAULT NULL,
  `size` int(11) NOT NULL,
  `numScans` int(11) DEFAULT NULL,
  `stationCode` varchar(2) DEFAULT NULL,
  `comment` text,
  `received` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

ALTER TABLE `FileData`
  ADD PRIMARY KEY (`id`),
  ADD KEY `experimentID` (`experimentID`);

ALTER TABLE `FileData`
  MODIFY `id` bigint(20) unsigned NOT NULL AUTO_INCREMENT;

ALTER TABLE `FileData` ADD FOREIGN KEY (`experimentID`) REFERENCES `difxdb`.`Experiment`(`id`) ON DELETE NO ACTION ON UPDATE NO ACTION;

INSERT INTO `difxdb`.`VersionHistory` (
`major` ,
`minor`
)
VALUES (
'1', '7'
);
