USE difxdb;

ALTER TABLE `Experiment` ADD `userID` BIGINT UNSIGNED NULL AFTER `vexfile`, ADD INDEX ( `userID` );
ALTER TABLE `Experiment` ADD `releasedByUserID` BIGINT UNSIGNED NULL AFTER `archivedByUserID` , ADD INDEX ( `releasedByUserID` ) ;


CREATE TABLE IF NOT EXISTS `User` (
  `id` bigint(20) unsigned NOT NULL AUTO_INCREMENT,
  `name` varchar(50) NOT NULL,
  `enabled` tinyint(1) NOT NULL DEFAULT '1',
  PRIMARY KEY (`id`)
) ENGINE=InnoDB  DEFAULT CHARSET=utf8 AUTO_INCREMENT=15 ;

ALTER TABLE `Experiment` ADD FOREIGN KEY ( `userID` ) REFERENCES `difxdb`.`User` (
`id`
) ON DELETE NO ACTION ON UPDATE NO ACTION ;

ALTER TABLE `Experiment` ADD FOREIGN KEY ( `releasedByUserID` ) REFERENCES `difxdb`.`User` (
`id`
) ON DELETE NO ACTION ON UPDATE NO ACTION ;

INSERT INTO `difxdb`.`VersionHistory` (
`major` ,
`minor`
)
VALUES (
'1', '3'
);
