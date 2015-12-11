/// <reference path="../typings/tsd.d.ts" />
/// <reference path="../manual-typings/es6-promise.d.ts" />
/// <reference path="../manual-typings/vdom-virtualize.d.ts" />
/// <reference path="../manual-typings/immutable.d.ts" />

import domToVdom from 'vdom-virtualize';
import { diff } from 'virtual-dom';
import { patch } from 'virtual-dom';
import { h } from 'virtual-dom';
import {
    Deploy, DeployRecord, createDeployRecord,
    Build, BuildRecord, createBuildRecord,
    DeployGroup, DeployGroupRecord, createDeployGroupRecord,
    DeployJson, BuildJson, Commit,
    GitHubCompareJson, GitHubCommitJson, GitHubCommit
} from './model';

import { List, Map, Record, Iterable } from 'immutable';

const { fetch } = window;

// https://github.com/Matt-Esch/virtual-dom/issues/276
const ih =
    (tagName: string,
     options: VirtualDOM.createProperties,
     children: List<VirtualDOM.VNode>): VirtualDOM.VNode => (
        h(tagName, options, children.toJS())
    );

let rootNode = document.body;
let currentTree = domToVdom(rootNode);

const updateDom = (newTree: VirtualDOM.VNode): any => {
    const patches = diff(currentTree, newTree);
    rootNode = patch(rootNode, patches);
    currentTree = newTree;
};

// A started deploy is all except status "Not running"
// This is because a deploy is not transactional, so we can't
// be sure if the box has updated or not once the job has started.
const hasDeployStarted = (deploy: DeployRecord) => deploy.status !== 'Not running';

const getStartedDeploysFor = (projectName: String, deploys: List<DeployRecord>) => (
    deploys
        .filter(hasDeployStarted)
        .filter(deploy => deploy.projectName === projectName)
        .sort((a, b) => a.time.getTime() - b.time.getTime())
);

const isDeployMostRecent = (deploy: Deploy, deploys: List<DeployRecord>): boolean => {
    const mostRecent = getStartedDeploysFor(deploy.projectName, deploys).last();
    return mostRecent ? (deploy.uuid === mostRecent.uuid) : false;
};

const getMostRecentDeploys = (deploys: List<DeployRecord>): List<DeployRecord> => (
    deploys.filter(deploy => isDeployMostRecent(deploy, deploys)).toList()
);

// Used in hyperscript because children cannot be booleans
// https://github.com/Matt-Esch/virtual-dom/issues/326
const exp = (condition: boolean): boolean | any => condition ? true : undefined;

const teamCityHost = 'http://teamcity.gu-web.net:8111';
const createBuildLink = (build: number) => (
    `${teamCityHost}/viewLog.html?buildNumber=${build}&buildTypeId=dotcom_master&tab=buildResultsDiv`
);

const riffRaffHost = 'https://riffraff.gutools.co.uk';
const createRiffRaffDeployLink = (uuid: string) => (
    `${riffRaffHost}/deployment/view/${uuid}`
);

const renderGroupDeployListNode = (deploys: List<DeployRecord>) => {
    const previousDeploysMap =
        Map<DeployRecord, DeployRecord>(
            deploys.map(deploy =>
                [deploy, getStartedDeploysFor(deploy.projectName, deploys)
                    .filterNot(d => d.uuid === deploy.uuid)
                    .last()
                ]
            )
        );

    const currentDeploys = getMostRecentDeploys(deploys);
    const notRunningDeploys = deploys.filter(deploy => !hasDeployStarted(deploy)).toList();

    const renderGroupDeployNodes = (groupDeploys: List<DeployRecord>, deployGroup: DeployGroupRecord) => {
        const shouldShowProjectNames = !groupDeploys.equals(currentDeploys);

        return h(
            'li',
            { className: `deploy deploy--${deployGroup.status.split(' ').join('-').toLowerCase()}` },
            [
                h('h3', [
                    h('a', { href: createBuildLink(deployGroup.build) }, `${deployGroup.build}`)
                ]),
                // Only show project names if we have multiple deployed groups
                exp(shouldShowProjectNames) && ih('ul', {}, groupDeploys
                    .sortBy(build => build.projectName)
                    .map(deploy => {
                        const previousBuild = previousDeploysMap.get(deploy);
                        return h('li', [
                            h('a', {
                                href: createRiffRaffDeployLink(deploy.uuid),
                                title: previousBuild ? `Previous build: ${previousBuild.build}` : ''
                            }, deploy.projectName)
                        ]);
                    })
                    .toList()
                )
            ]
        );
    };

    const createDeployGroup = (deploys: List<DeployRecord>) => (
        deploys.groupBy(deploy => (
            createDeployGroupRecord({
                status: deploy.status,
                build: deploy.build
            })
        ))
    );

    const currentDeployGroupNodes = createDeployGroup(currentDeploys)
        .map(renderGroupDeployNodes)
        .toList();

    return h('div', {}, [
        ih('ul', { className: 'deploys' }, currentDeployGroupNodes),
        exp(notRunningDeploys.size > 0) && [
            h('h3', 'Queue'),
            ih('ul', {}, createDeployGroup(notRunningDeploys)
                .map((groupDeploys, deployGroup) => (
                    h('li', [
                        h('strong', [
                            h('a', { href: createBuildLink(deployGroup.build) }, deployGroup.build.toString())
                        ]),
                        ih('ul', {}, (
                            groupDeploys
                                .sortBy(build => build.projectName)
                                .map(deploy => {
                                    const previousBuild = previousDeploysMap.get(deploy);
                                    return h('li', [
                                        h('a', {
                                            href: createRiffRaffDeployLink(deploy.uuid),
                                            title: previousBuild ? `Previous build: ${previousBuild.build}` : ''

                                        }, deploy.projectName)
                                    ]);
                                })
                                .toList()
                        ))
                    ])
                ))
                .toList()
            )
        ]
    ]);
};

const renderPage: (
    deploysPair: [ List<DeployRecord>, List<DeployRecord> ],
    // TODO: Use tuple instead
    deployPair: Array<DeployRecord>,
    commits: Array<GitHubCommit>
) => VirtualDOM.VNode =
    (
        [ codeDeploys, prodDeploys ],
        [ latestCodeDeploy, oldestProdDeploy ],
        commits
    ) => {
        const isInSync = oldestProdDeploy.build === latestCodeDeploy.build;
        return h('body', {}, [
            h('h1', `Status: ${isInSync ? 'in sync. Ship it!' : 'out of sync.'}`),
            exp(commits.length > 0) && h('div', [
                h('h2', [
                    'Difference (',
                    h('span', { title: 'Oldest PROD deploy' }, `${oldestProdDeploy.build}`),
                    '...',
                    h('span', { title: 'Latest CODE deploy' }, `${latestCodeDeploy.build}`),
                    ')'
                ]),
                // TODO: Group consecutive by author
                h('ul', (
                    commits.map(commit => (h('li', [
                        h('a', { href: commit.url }, commit.message),
                        ` by ${commit.authorName}`
                    ]))
                )))
            ]),

            h('h2', 'CODE'),
            renderGroupDeployListNode(codeDeploys),
            h('h2', 'PROD'),
            renderGroupDeployListNode(prodDeploys)
        ])
    }

const apiPath = '/deploys-radiator/api';
const getDeploys = (stage: string): Promise<List<DeployRecord>> => (
    fetch(`${apiPath}/deploys?projectName=dotcom:&stage=${stage}&pageSize=200`, { credentials: 'same-origin' })
        .then((response): Promise<Array<DeployJson>> => response.json())
        .then(deploys => List(deploys.map(deploy => createDeployRecord({
            build: Number(deploy.build),
            uuid: deploy.uuid,
            projectName: deploy.projectName.replace(/^dotcom:/, ''),
            status: deploy.status,
            time: new Date(deploy.time)
        }))))
);
const getBuild = (build: number): Promise<BuildRecord> => (
    fetch(`${apiPath}/builds/${build}`, { credentials: 'same-origin' })
        .then((response): Promise<BuildJson> => response.json())
        .then(build => createBuildRecord({
            number: Number(build.number),
            projectName: build.projectName,
            commits: build.commits
        }))
);

const gitHubApiHost = 'https://api.github.com';
const getDifference = (base: string, head: string): Promise<Array<GitHubCommit>> => (
    fetch(`${gitHubApiHost}/repos/guardian/frontend/compare/${base}...${head}`)
        .then((response): Promise<GitHubCompareJson> => response.json())
        .then(json => json.commits)
        .then(gitHubCommitsJson => gitHubCommitsJson.map((gitHubCommitJson): GitHubCommit => (
            {
                url: gitHubCommitJson.html_url,
                authorName: gitHubCommitJson.commit.author.name,
                message: gitHubCommitJson.commit.message
            }
        )))
);

const run = () => {
    const deploysPromise = Promise.all([
        getDeploys('CODE'),
        getDeploys('PROD')
    ]);

    const deployRefsPromise = deploysPromise.then(([ codeDeploys, prodDeploys ]) => {
        const currentCodeDeploys = getMostRecentDeploys(codeDeploys);
        const currentProdDeploys = getMostRecentDeploys(prodDeploys);
        const latestCodeDeploy = currentCodeDeploys
            .sortBy(deploy => deploy.build)
            .last();
        const blacklistProdDeploys = List(['static', 'router', 'training-preview', 'preview']);
        const oldestProdDeploy = currentProdDeploys
            .filter(deploy => !blacklistProdDeploys.contains(deploy.projectName))
            .sortBy(deploy => deploy.build)
            .first();

        return [ latestCodeDeploy, oldestProdDeploy ];
    });

    const buildsPromise = deployRefsPromise.then(([ latestCodeDeploy, oldestProdDeploy ]) => (
        Promise.all([
            getBuild(latestCodeDeploy.build),
            getBuild(oldestProdDeploy.build),
        ])
    ));

    const differencePromise = buildsPromise.then(([ codeBuild, prodBuild ]) => {
        // TODO: Don't lookup
        // List().headOption
        const prodCommit = prodBuild.commits[0].sha;
        const codeCommit = codeBuild.commits[0].sha;
        // This assumes prod comes before code
        return getDifference(prodCommit, codeCommit).then(gitHubCommits => gitHubCommits.reverse());
    });

    Promise.all([ deploysPromise, deployRefsPromise, differencePromise ])
        .then(([ deploysPair, deployPair, commits ]) => renderPage(deploysPair, deployPair, commits))
        .then(updateDom);
};


const intervalSeconds = 10;
setInterval(run, intervalSeconds * 1000);
run();
