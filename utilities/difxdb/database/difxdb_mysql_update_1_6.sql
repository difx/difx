USE difxdb;

ALTER TABLE `Experiment` ADD `dateObserved` DATE NULL DEFAULT NULL AFTER `dateCreated`;

INSERT INTO `difxdb`.`VersionHistory` (
`major` ,
`minor`
)
VALUES (
'1', '6'
);
