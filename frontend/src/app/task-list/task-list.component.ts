import { Component, OnInit } from '@angular/core';
import { Http } from '@angular/http';
import { PushNotificationsService } from 'angular2-notifications';

@Component({
    selector: 'app-task-list',
    templateUrl: './task-list.component.html',
    styleUrls: ['./task-list.component.css']
})
export class TaskListComponent implements OnInit {
    tasks = [];
    doneTasks = [];
    timerToken;

    constructor(private http: Http, private _pushNotifications: PushNotificationsService) {
    }

    ngOnInit() {
        this.fetchTasks();
    }

    notify() {
        let msg = '';
        if (this.tasks && this.tasks[0] && this.tasks[0].name) {
            msg = 'next task: ' + this.tasks[0].name;
        } else {
            msg = 'Nothing to do! Just enjoy life!';
        }

        const options = {
            body: msg,
            icon: '/static/image/happyscheduler-logo.png',
            requireInteraction: true
        };

        this._pushNotifications.create('happy scheduler', options).subscribe(
            res => {},
            err => console.log(err)
        );
    }

    public fetchTasks() {
        if (this.timerToken) {
            clearTimeout(this.timerToken);
        }
        this.timerToken =
            setInterval(
                () => this.notify(),
                (12 * 1000 * 60 * 60)   // every 12 hours
            );

        this.http.get('../tasks').subscribe(
            data => this.tasks = JSON.parse(data['_body']).tasks,
            error => alert('An error occurred: ' + error)
        );
        this.http.get('../tasks/done').subscribe(
            data => this.doneTasks = JSON.parse(data['_body']).tasks,
            error => alert('An error occurred: ' + error)
        );
    }

    onDeleteClick(id: number) {
        this.http.delete('../tasks/' + id).subscribe(
            () => this.fetchTasks(),
            error => alert('An error occurred: ' + error)
        );
    }

    onDoneClick(id: number) {
        const taskToUpdate = this.tasks.find((task) => { return task.id === id; });
        taskToUpdate.done = true;
        this.http.put('../tasks/' + id, JSON.stringify(taskToUpdate)).subscribe(
            () => this.fetchTasks(),
            error => alert('An error occurred: ' + error)
        );
    }
}
