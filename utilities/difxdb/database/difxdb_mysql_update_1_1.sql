USE difxdb;

ALTER TABLE `Experiment` ADD `dateArchived` DATETIME NULL ;
ALTER TABLE `Experiment` ADD `dateReleased` DATETIME NULL ;
ALTER TABLE `Experiment` ADD `archivedBy` VARCHAR(30) NULL ;
ALTER TABLE `Experiment` ADD `releasedBy` VARCHAR(30) NULL ;
ALTER TABLE `Experiment` ADD `comment` TEXT NULL ;

INSERT INTO `difxdb`.`VersionHistory` (
`major` ,
`minor`
)
VALUES (
'1', '1'
);

