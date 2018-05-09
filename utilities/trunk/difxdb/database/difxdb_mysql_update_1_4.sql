USE difxdb;

ALTER TABLE `Experiment` ADD `emailnotification` VARCHAR(255) NULL DEFAULT NULL AFTER `userID`;

INSERT INTO `difxdb`.`VersionHistory` (
`major` ,
`minor`
)
VALUES (
'1', '4'
);
