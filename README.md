# PR-Notifier

Receive regular notifications about your team's pull requests which you need to review.

Note: MacOs only.

## Motivation

All I wanted was a tool that would pop up a window, after every x amount of time, to show me PRs that I need to review.
Going through emails and slack messages wasn't helping enough.
Also it was a good excuse to write my first Haskell application, which means this code is far from perfect or following best practices.
I'm happy to get some feedback!

## How does it work?

Basically you run `pr-notifier --setup`, then you answer a set of questions like: bitbucket username, password, team members (their bitbucket usernames), reviewer (your username) and an interval (how often you get the notifications).
Those answers are then saved in a config file under your home directory: `/Users/foo/.pr-notifier`.

That config is used to:
- make a get request to [Bitbucket's server rest api](https://docs.atlassian.com/bitbucket-server/rest/6.10.0/bitbucket-rest.html) to get the pull requests;
- create a [plist](https://en.wikipedia.org/wiki/Property_list) file which are used by [launchd](https://en.wikipedia.org/wiki/Launchd) to schedule the application. The plist file is saved under `/Users/Library/LaunchAgents/uk.co.brittes.pr-notifier.plist`;
- and filter the PRs based on the team members.

After that a bit of [AppleScript](https://en.wikipedia.org/wiki/AppleScript) is called to display the PRs. You can then select them and it will open the links on your browser.

## Installation

1- Download the executable from this repo's [download page](https://bitbucket.org/narley/pr-notifier/downloads/);

(Optional): move the executable to `/Users/your_user_name/.local/`

2- Configure the application:
```bash
pr-notifier --setup
```

## Usage

- Display help
```bash
pr-notifier --help
```

- Setup application
```bash
pr-notifier --setup
```

- Single run: one time execution (will not schedule and config file must exist)
```bash
pr-notifier --run
```

- Create plist file
```bash
pr-notifier --create-plist
```

- Load plist file: will schedule `pr-notifier --run` to run X amount of seconds
```bash
pr-notifier --load-plist
```

- Unload plist file: will delete schedule
```bash
pr-notifier --unload-plist
```

- Delete plist file: if using this command make sure you first unload plist file
```bash
pr-notifier --delete-plist
```

## Contributing
Pull requests are welcome. For major changes, please open an issue first to discuss what you would like to change.
